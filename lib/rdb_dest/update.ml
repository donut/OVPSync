
open Lwt.Infix

module type DBC = Caqti_lwt.CONNECTION
module Bopt = BatOption

exception Missing_id of string * string

let lplf fmt = Printf.ksprintf (Lwt_io.printl) fmt

module Q = struct 

  module Creq = Caqti_request
  open Caqti_type

  let or_insert_source = Creq.exec 
    (tup4 string string (option int) (tup2 ptime ptime))
    "INSERT INTO source (name, media_id, video_id, added, modified) \
     VALUES (?, ?, ?, ?, ?) \
     ON DUPLICATE KEY UPDATE \
      video_id = VALUES(video_id), \
      added = VALUES(added), modified = VALUES(modified)"

  let video = Creq.exec
          (* id title slug publish *)
    (tup4 (tup3 string string ptime)
          (* expires file_uri md5 width *)
          (tup4 (option ptime) (option string) (option string) (option int))
          (* height duration thumbnail_uri description *)
          (tup4 (option int) (option int) (option string) (option string))
          (* cms_id link canonical_source_id id *)
          (tup4 (option string) (option string) int int))
    "UPDATE video SET \
      title = ?, slug = ?, publish = ?, expires = ?, \
      file_uri = ?, md5 = ?, width = ?, height = ?, \
      duration = ?, thumbnail_uri = ?, description = ?, cms_id = ?, link = ?, \
      canonical_source_id = ? \
     WHERE id = ? LIMIT 1"

  let video_thumbnail_uri = Creq.exec
    (tup2 string int)
    "UPDATE video SET thumbnail_uri = ? WHERE id = ? LIMIT 1"

  let video_file_uri = Creq.exec
    (tup2 string int)
    "UPDATE video SET file_uri = ? WHERE id = ? LIMIT 1"
    
  let video_md5 = Creq.exec
    (tup2 string int)
    "UPDATE video SET md5 = ? WHERE id = ? LIMIT 1"

end

let or_insert_x_fields pl x x_id fields =
  let x_name = match x with `Source -> "source" | `Video -> "video" in
  let module D = Dynaparam in
  let (D.Pack (typ, vals, placeholders)) = List.fold_left 
    (fun pack (name, value) ->
      D.add Caqti_type.(tup3 int string string)
            (x_id, name, value) "(?, ?, ?)" pack)
    D.empty fields in
  let placeholders = String.concat ", " placeholders in
  let sql = Printf.sprintf
    "INSERT INTO %s_field (%s_id, name, value) VALUES %s \
     ON DUPLICATE KEY UPDATE value = VALUES(value)"
    x_name x_name placeholders in
  let query = Caqti_request.exec typ sql in
  Util.exec pl query vals

let or_insert_source_fields pl src_id fields =
  or_insert_x_fields pl `Source src_id fields

let or_insert_source pl src =
  let added_ts = Source.added src |> Util.ptime_of_int in
  let modified_ts = Source.modified src |> Util.ptime_of_int in
  let values =
    Source.(name src, media_id src, video_id src, (added_ts, modified_ts)) in
  Util.exec pl Q.or_insert_source values >>= fun () ->

  let%lwt id = match Source.id src with
  | Some id -> Lwt.return id
  | None ->
    let name, media_id = Source.(name src, media_id src) in
    begin match%lwt Select.source_id pl ~name ~media_id with
    | None ->
      let id = Printf.sprintf "ovp:%s media_id:%s" name media_id in
      raise (Util.Row_not_found id)
    | Some id -> Lwt.return id
    end
  in
  
  let fields = Source.custom src in 
  let field_names = List.map fst fields in
  Delete.source_fields_not_named pl id field_names >>= fun () ->

  or_insert_source_fields pl id fields >>= fun () ->

  Lwt.return { src with id = Some id }

let sources_of_video pl vid =
  let canonical = Video.canonical vid in
  let is_canonical = Source.are_same canonical in
  let sources = 
    let lst = Video.sources vid in
    let includes_canonical = List.exists is_canonical lst in
    (if includes_canonical then lst else canonical :: lst)
    |> List.map (fun s -> Source.({ s with video_id = (Video.id vid) }))
  in
  (* [or_insert_sources] will add IDs if an INSERT was performed. *)
  Lwt_list.map_s (or_insert_source pl) sources >>= fun sources ->

  let canonical = List.find is_canonical sources in
  Lwt.return { vid with canonical; sources }

let or_insert_video_fields pl vid_id fields =
  or_insert_x_fields pl `Video vid_id fields

let video pl vid =
  let canonical = Video.canonical vid in

  let vid_id = match Video.id vid with
  | None ->
    let str_id = Printf.sprintf "ovp:%s media_id:%s"
      (Source.name canonical) (Source.media_id canonical) in
    raise @@ Missing_id ("video.id", str_id)
  | Some id -> id
  in

  sources_of_video pl vid >>= fun vid ->

  let Video.{ canonical; tags; _ } = vid in

  Insert.new_tags_of pl tags >>= fun () ->

  let fields = Video.custom vid in
  let field_names = fields |> List.map fst in
  Delete.video_fields_not_named pl vid_id field_names >>= fun () ->
  or_insert_video_fields pl vid_id fields >>= fun () ->

  let publish_ts = Video.publish vid |> Util.ptime_of_int in
  let expires_ts = Video.expires vid |> Bopt.map Util.ptime_of_int in
  let canonical_id = Bopt.get @@ Source.id canonical in
  let file_str = Video.file_uri vid |> Bopt.map Uri.to_string in
  let thumb_str = Video.thumbnail_uri vid |> Bopt.map Uri.to_string in
  let link_str = Video.link vid |> Bopt.map Uri.to_string in
  Util.exec pl Q.video
    Video.( (title vid, slug vid, publish_ts)
          , (expires_ts, file_str, md5 vid, width vid)
          , (height vid, duration vid, thumb_str, description vid)
          , (cms_id vid, link_str, canonical_id, vid_id) )
    >>= fun () ->

  Lwt.return vid

let video_thumbnail_uri pl vid_id uri =
  Util.exec pl Q.video_thumbnail_uri (uri, vid_id)

let video_file_uri pl vid_id uri =
  Util.exec pl Q.video_file_uri (uri, vid_id)

let video_md5 pl vid_id md5 =
  Util.exec pl Q.video_md5 (md5, vid_id)