
open Lwt.Infix

module type DBC = Caqti_lwt.CONNECTION

exception Already_inserted of string * int

module Q = struct 

  module Creq = Caqti_request
  open Caqti_type

  let last_insert_id = Creq.find unit int "SELECT LAST_INSERT_ID()"

  let video = Creq.exec
          (* title slug publish *)
    (tup4 (tup3 string string int)
          (* expires file_uri md5 width *)
          (tup4 (option int) (option string) (option string) (option int))
          (* height duration thumbnail_uri description *)
          (tup4 (option int) (option int) (option string) (option string))
          (* cms_id link canonical_source_id *)
          (tup3 (option string) (option string) int))
    "INSERT INTO video \
      ( title, slug, publish, expires, file_uri, md5, width, height \
      , duration, thumbnail_uri, description, cms_id, link \
      , canonical_source_id ) \
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

  let source = Creq.exec
    (tup4 string string (option int) (tup2 int int))
    "INSERT INTO source (name, media_id, video_id, added, modified) \
     VALUES (?, ?, ?, ?, ?)"

  let source_field = Creq.exec
    (tup3 int string string)
    "INSERT INTO source_field (source_id, name, value) VALUES (?, ?, ?)"

  let or_update_source_field = Creq.exec
    (tup3 int string string)
    "INSERT INTO source_field (source_id, name, value) VALUES (?, ?, ?) \
     ON DUPLICATE KEY UPDATE value = VALUES(value)"

  let source_video_id = Creq.exec
    (tup2 int int)
    "UPDATE source_field SET video_id = ? WHERE id = ?"

end

let source_fields (module DB : DBC) source_id fields =
  let module D = Dynaparam in
  let (D.Pack (typ, values, placeholders)) = List.fold_left (fun pk (n, v) ->
    D.add Caqti_type.(tup3 int string string) (source_id, n, v) "(?, ?, ?)" pk
  ) D.empty fields
  in
  let placeholders = String.concat ", " placeholders in
  let sql = Printf.sprintf
    "INSERT INTO source_field (source_id, name, value) VALUES %s" placeholders
  in
  let query = Caqti_request.exec ~oneshot:true typ sql in
  DB.exec query values >>= Caqti_lwt.or_fail

let source (module DB : DBC) src =
  DB.exec Q.source
    Source.(name src, media_id src, video_id src, (added src, modified src))
    >>= Caqti_lwt.or_fail >>= fun () ->
  let%lwt id = DB.find Q.last_insert_id () >>= Caqti_lwt.or_fail in
  source_fields (module DB) id (Source.custom src) >>= fun () ->
  Lwt.return { src with id = Some id }

let tags (module DB : DBC) lst = 
  (* Existing names are filtered out to void incrementing the autoincrement
   * counter, as `ON DUPLICATE KEY UPDATE` does. This process is expected to
   * be run many times with already existing tags, since there is a reliatvely
   * small pool of tags but a large number of videos. *)
  let%lwt existing = Select.tags_by_name (module DB) lst in
  let names = lst |> List.filter
    (fun n -> not @@ List.exists (fun (_, n') -> n == n') existing) in
  let module D = Dynaparam in
  let (D.Pack (typ, values, placeholders)) = List.fold_left
    (fun pack tag -> D.add Caqti_type.string tag "(?)" pack) D.empty names in
  let sql = Printf.sprintf
    "INSERT INTO tag (name) VALUES %s ON DUPLICATE KEY UPDATE id=id"
    (String.concat ", " placeholders) in
  let query = Caqti_request.exec typ sql in
  DB.exec query values >>= Caqti_lwt.or_fail

let video_tag_relations (module DB : DBC) video_id tag_ids =
  let module D = Dynaparam in
  let (D.Pack (typ, values, placeholders)) = List.fold_left
    (fun pack tag_id ->
      D.add Caqti_type.(tup2 int int) (video_id, tag_id) "(?, ?)" pack) 
    D.empty tag_ids in
  let sql = Printf.sprintf
    "INSERT INTO video_tag (video_id, tag_id) VALUES %s"
    (String.concat ", " placeholders) in
  let query = Caqti_request.exec typ sql in
  DB.exec query values >>= Caqti_lwt.or_fail

let video_fields (module DB : DBC) video_id fields =
  let module D = Dynaparam in
  let (D.Pack (typ, values, placeholders)) = List.fold_left
    (fun pk (n, v) ->
      D.add Caqti_type.(tup3 int string string) (video_id, n, v) "(?, ?, ?)" pk)
    D.empty fields in
  let placeholders = String.concat ", " placeholders in
  let sql = Printf.sprintf
    "INSERT INTO video_field (video_id, name, value) VALUES %s" placeholders in
  let query = Caqti_request.exec ~oneshot:true typ sql in
  DB.exec query values >>= Caqti_lwt.or_fail

let video (module DB : DBC) vid =
  begin if Video.id vid |> BatOption.is_some then
    raise @@ Already_inserted ("video", Video.id vid |> BatOption.get)
  end;

  let canonical = Video.canonical vid in
  let sources   = Video.sources vid in
  let match_canonical s = 
    Source.(name s == name canonical && media_id s == media_id canonical)
  in
  let sources_include_canonical = sources |> List.exists match_canonical in
  let sources =
    if sources_include_canonical then sources else canonical :: sources
  in
  let%lwt sources = sources |> Lwt_list.map_s (fun s ->
    match Source.id s with
    | Some _ -> Lwt.return s
    | None -> source (module DB) s
  ) in
  let canonical = sources |> List.find match_canonical in
  
  let canonical_id = BatOption.get @@ Source.id canonical in
  let some_string_of_uri uri =
    if uri == Uri.empty then None else Some (Uri.to_string uri)
  in
  let file_str = some_string_of_uri @@ Video.file_uri vid in
  let thumb_str = some_string_of_uri @@ Video.thumbnail_uri vid in
  let link_str = Video.link vid
    |> BatOption.map_default some_string_of_uri None
  in
  DB.exec Q.video
    Video.( (title vid, slug vid, publish vid)
          , (expires vid, file_str, md5 vid, width vid)
          , (height vid, duration vid, thumb_str, description vid)
          , (cms_id vid, link_str, canonical_id) )
    >>= Caqti_lwt.or_fail >>= fun () ->

  let%lwt vid_id = DB.find Q.last_insert_id () >>= Caqti_lwt.or_fail in

  sources |> Lwt_list.iter_s (fun s ->
    let id = Source.id s |> BatOption.get in
    DB.exec Q.source_video_id (vid_id, id) >>= Caqti_lwt.or_fail)
    >>= fun () ->
  let sources =
    List.map (fun s -> Source.({ s with video_id = Some vid_id })) sources
  in
  let canonical = List.find match_canonical sources in
  
  tags (module DB) (Video.tags vid) >>= fun () ->
  let%lwt tag_list = Select.tags_by_name (module DB) (Video.tags vid) in
  let tag_ids = List.map fst tag_list in
  video_tag_relations (module DB) vid_id tag_ids >>= fun () ->
  
  video_fields (module DB) vid_id (Video.custom vid) >>= fun () ->

  Lwt.return { vid with id = Some vid_id; canonical; sources }