type tile = { path : Eio.Fs.dir_ty Eio.Path.t; info : Oclas.Las.t }

let ( let* ) = Result.bind

(* For the rtree we avoid storing complex data in there so we don't need to have
a Repr of anything other than basic types. *)

module RTile = struct
  type t = {
    name : string;
    envelope : Rtree.Rectangle.t;
    count : int;
    bounds : (float * float * float) * (float * float * float);
  }

  let v tile =
    let _, name = Option.get (Eio.Path.split tile.path) in
    let header = Oclas.Las.header tile.info in
    let bounds = Oclas.Header.bounds header in
    let (x0, y0, _), (x1, y1, _) = bounds in
    let envelope = Rtree.Rectangle.v ~x0 ~y0 ~x1 ~y1 in
    let count = Oclas.Header.number_of_point_records header in
    { name; envelope; count; bounds }

  let t =
    let open Repr in
    record "RTile" (fun n e c b ->
        { name = n; envelope = e; count = c; bounds = b })
    |+ field "name" string (fun t -> t.name)
    |+ field "envelope" Rtree.Rectangle.t (fun t -> t.envelope)
    |+ field "count" int (fun t -> t.count)
    |+ field "bounds"
         (pair (triple float float float) (triple float float float))
         (fun t -> t.bounds)
    |> sealr

  type envelope = Rtree.Rectangle.t

  let name t = t.name
  let bounds t = t.bounds
  let point_count t = t.count
  let envelope t = t.envelope
end

module R = Rtree.Make (Rtree.Rectangle) (RTile)

type state = { tile_map : (string * tile) list; rtree : R.t }

let build_state tile_list =
  let tile_map =
    List.map
      (fun tile ->
        let _, name = Option.get (Eio.Path.split tile.path) in
        (name, tile))
      tile_list
  in
  let rtiles = List.map (fun (_, tile) -> RTile.v tile) tile_map in
  let rtree = R.load rtiles in
  { tile_map; rtree }

let rec find_las_files sw dir_path =
  Eio.Path.read_dir dir_path
  |> List.concat_map (fun item ->
      let path = Eio.Path.(dir_path / item) in
      match Eio.Path.is_directory path with
      | true -> find_las_files sw path
      | false -> (
          match String.ends_with ~suffix:".laz" item with
          | false -> []
          | true -> (
              let flow = Eio.Path.open_in ~sw path in
              let buf = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
              let info = Oclas.Las.of_buffer buf in
              match info with Ok info -> [ { path; info } ] | Error _ -> [])))

let rtiles_to_json rtiles =
  `List
    (List.map
       (fun rtile ->
         let name = RTile.name rtile in
         let (x0, y0, z0), (x1, y1, z1) = RTile.bounds rtile in
         `Assoc
           [
             ("name", `String name);
             ("point_count", `Int (RTile.point_count rtile));
             ( "bounds",
               `Assoc
                 [
                   ( "min",
                     `Assoc
                       [ ("x", `Float x0); ("y", `Float y0); ("z", `Float z0) ]
                   );
                   ( "max",
                     `Assoc
                       [ ("x", `Float x1); ("y", `Float y1); ("z", `Float z1) ]
                   );
                 ] );
           ])
       rtiles)
  |> Yojson.Safe.to_string

let get_float_query uri name =
  match Uri.get_query_param uri name with
  | None -> Error "Missing query"
  | Some x -> (
      match Float.of_string_opt x with
      | None -> Error "Invalid float"
      | Some x -> Ok x)

let get_point_query state req =
  let uri = Uri.of_string (Http.Request.resource req) in
  let* x = get_float_query uri "x" in
  let* y = get_float_query uri "y" in
  let envelope = Rtree.Rectangle.v ~x0:x ~y0:y ~x1:(x +. 1.) ~y1:(y +. 1.) in
  let rtiles = R.find state.rtree envelope in
  Ok rtiles

let render_index state _req =
  let all_tiles = R.values state.rtree in
  Printf.printf "%d\n%!" (List.length all_tiles);
  let body = rtiles_to_json all_tiles in
  Cohttp_eio.Server.respond_string ~status:`OK ~body ()

let render_find state req =
  let open Cohttp_eio in
  match get_point_query state req with
  | Error str -> Server.respond_string ~status:`Not_acceptable ~body:str ()
  | Ok tiles ->
      let body = rtiles_to_json tiles in
      Server.respond_string ~status:`OK ~body ()

let render_static_file sw path _state req =
  let stat = Eio.Path.stat ~follow:true path in
  let mtime = Option.get (Ptime.of_float_s stat.mtime) in
  let last_modified = Util.ptime_to_last_modified mtime in
  let content_type = Magic_mime.lookup (Eio.Path.native_exn path) in
  let headers =
    Http.Header.of_list
      [
        ("Accept-Ranges", "bytes");
        ("Content-Length", Optint.Int63.to_string stat.size);
        ("Content-Type", content_type);
        ("Last-Modified", last_modified);
      ]
  in

  let file = Eio.Path.open_in ~sw path in

  let request_headers = Http.Request.headers req in
  let range_header = Http.Header.get request_headers "range" in
  let file_size = Optint.Int63.to_int64 stat.size in
  match range_header with
  | Some range_str when String.starts_with ~prefix:"bytes=" range_str -> (
      match Util.parse_range range_str file_size with
      | Some (start_pos, end_pos) ->
          let len = Int64.sub end_pos start_pos in
          let _ = Eio.File.seek file (Optint.Int63.of_int64 start_pos) `Set in
          let buf = Eio.Buf_read.of_flow file ~max_size:(Int64.to_int len) in
          let bytes = Eio.Buf_read.take (Int64.to_int len) buf in
          let body = Eio.Flow.string_source bytes in
          Cohttp_eio.Server.respond ~status:`Partial_content ~headers ~body ()
      | None -> Cohttp_eio.Server.respond ~status:`OK ~headers ~body:file ())
  | _ -> Cohttp_eio.Server.respond ~status:`OK ~headers ~body:file ()

let handler routes state _socket req _body =
  let open Cohttp_eio in
  let uri = Uri.of_string (Http.Request.resource req) in
  let path = Uri.path uri in

  match Http.Request.meth req with
  | `GET -> (
      match List.assoc_opt path routes with
      | Some handler -> handler state req
      | None -> Server.respond_string ~status:`Not_found ~body:"Not found\n" ())
  | _ -> Server.respond_string ~status:`Not_found ~body:"Not found\n" ()

let log_error ex = Printf.eprintf "Server error: %s\n%!" (Printexc.to_string ex)

let rec build_static_routes sw dir_path =
  Eio.Path.read_dir dir_path
  |> List.concat_map (fun item ->
      let path = Eio.Path.(dir_path / item) in
      match Eio.Path.is_directory path with
      | true ->
          build_static_routes sw path
          |> List.map (fun (name, handler) ->
              let new_name = "/" ^ item ^ name in
              (new_name, handler))
      | false -> [ ("/" ^ item, render_static_file sw path) ])

let get_static_files env sw path =
  let arg_path =
    if Filename.is_relative path then Eio.Path.(env#cwd / path)
    else Eio.Path.(env#fs / path)
  in
  build_static_routes sw arg_path

let laserver env sw laz_path static_path =
  let arg_laz_path =
    if Filename.is_relative laz_path then Eio.Path.(env#cwd / laz_path)
    else Eio.Path.(env#fs / laz_path)
  in
  let dir = Eio.Path.open_dir ~sw arg_laz_path in
  let tiles = find_las_files sw dir in
  Printf.printf "Found %d tiles\n" (List.length tiles);
  let state = build_state tiles in
  Printf.printf "rtree size: %d\n" (R.size state.rtree);
  Printf.printf "Rtree values: %d\n" (List.length (R.values state.rtree));

  let fixed_routes = [ ("/api/all", render_index); ("/api/find", render_find) ] in

  (* This uses the tile list rather than the rtree as I don't know how
  to put the eio path into the Repr rtree requires *)
  let laz_static_routes =
    List.map
      (fun (name, tile) -> ("/tile/" ^ name, render_static_file sw tile.path))
      state.tile_map
  in

  let static_routes =
    match static_path with
    | Some path -> get_static_files env sw path
    | None -> []
  in

  let routes = fixed_routes @ laz_static_routes @ static_routes in

  let socket =
    Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
  in
  Printf.printf "Listening on http://localhost:8080\n%!";
  Cohttp_eio.Server.run socket ~on_error:log_error
    (Cohttp_eio.Server.make ~callback:(handler routes state) ())

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Array.to_list Sys.argv with
  | [ _; laz_path; static_path ] -> laserver env sw laz_path (Some static_path)
  | [ _; laz_path ] -> laserver env sw laz_path None
  | _ -> Printf.eprintf "Usage: %s <directory>\n" Sys.argv.(0)
