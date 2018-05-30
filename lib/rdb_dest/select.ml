
open Lwt.Infix

module type DBC = Caqti_lwt.CONNECTION
module Bopt = BatOption

module Q = struct 

  module Creq = Caqti_request
  open Caqti_type

  let source_type = (tup3 (tup4 int string string (option int)) ptime ptime)

  let source = Creq.find_opt
    (tup2 string string) source_type
    "SELECT id, name, media_id, video_id, added, modified \
       FROM source WHERE name = ? AND media_id = ? LIMIT 1"

  let source_id = Creq.find_opt
    (tup2 string string) int
    "SELECT id FROM source WHERE name = ? AND media_id = ? LIMIT 1"

  let source_fields = Creq.collect
    int (tup2 string string)
    "SELECT name, value FROM source_field WHERE source_id = ? ORDER BY name"

  let source_fields_by_video_id = Creq.collect
    int (tup3 int string string)
    "SELECT sf.source_id, sf.name, sf.value FROM source_field AS sf \
       JOIN source AS s ON s.id = sf.source_id \
      WHERE s.video_id = ?"

  let sources_by_video_id = Creq.collect
    int source_type
    "SELECT id, name, media_id, video_id, created, updated \
       FROM source WHERE video_id = ?"

  let video = Creq.find_opt
    int
          (* title slug publish *)
    (tup4 (tup3 string string ptime)
          (* expires file_uri md5 width *)
          (tup4 (option ptime) (option string) (option string) (option int))
          (* height duration thumbnail_uri description *)
          (tup4 (option int) (option int) (option string) (option string))
          (* cms_id link canonical_source_id created updated *)
          (tup4 (option string) (option string) int (tup2 ptime ptime)))
    "SELECT title, slug, publish, expires, file_uri, md5, width, height \
          , duration, thumbnail_uri, description, cms_id, link \
          , canonical_source_id, created, updated \
       FROM video WHERE id = ? LIMIT 1"

  let video_fields = Creq.collect
    int (tup2 string string)
    "SELECT name, value FROM video_field WHERE video_id = ? ORDER BY name"

  let video_tags = Creq.collect
    int string
    "SELECT tag.name FROM video_tag \
       JOIN tag ON tag.id = video_tag.tag_id \
      WHERE video_tag.video_id = ? \
      ORDER BY tag.name"

end


let source_of_row row ~custom =
  let ((id, name, media_id, video_id), added_pt, modified_pt) = row in
  let added = added_pt |> Util.int_of_ptime in
  let modified = modified_pt |> Util.int_of_ptime in
  { Source. id = Some id; name; media_id; video_id; custom; added; modified }

let source_fields (module DB : DBC) source_id =
  DB.collect_list Q.source_fields source_id >>= Caqti_lwt.or_fail

let source (module DB : DBC) ~name ~media_id =
  DB.find_opt Q.source (name, media_id) >>= Caqti_lwt.or_fail >>= function
  | None -> Lwt.return None
  | Some row ->
    let id = BatTuple.(Tuple3.first row |> Tuple4.first) in
    let%lwt custom = source_fields (module DB) id in
    Lwt.return @@ Some (source_of_row row ~custom)

let source_id (module DB : DBC) ~name ~media_id =
  DB.find_opt Q.source_id (name, media_id) >>= Caqti_lwt.or_fail

let source_fields_by_video_id (module DB : DBC) video_id =
  let%lwt fields = DB.collect_list Q.source_fields_by_video_id video_id
    >>= Caqti_lwt.or_fail in
  fields
    |> BatList.group (fun (a, _, _) (b, _, _) -> compare a b)
    |> List.map (fun l ->
      let source_id = List.hd l |> BatTuple.Tuple3.first in
      let l' = List.map (fun (_, n, v) -> (n, v)) l in
      (source_id, l'))
    |> Lwt.return

let sources_by_video_id (module DB : DBC) video_id =
  let%lwt sources = DB.collect_list Q.sources_by_video_id video_id
    >>= Caqti_lwt.or_fail in
  let%lwt fields = source_fields_by_video_id (module DB) video_id in
  sources
    |> List.map (fun row ->
      let id = BatTuple.(Tuple3.first row |> Tuple4.first) in
      let custom = List.assoc_opt id fields |> Bopt.default [] in
      source_of_row row ~custom)
    |> Lwt.return

let tags_by_name (module DB : DBC) names = 
  let module D = Dynaparam in
  let (D.Pack (typ, values, plist)) = List.fold_left
    (fun pack name -> D.add Caqti_type.string name "?" pack)
    D.empty names in
  let placeholders = String.concat ", " plist in
  let sql = Printf.sprintf 
    "SELECT id, name FROM tag WHERE name IN (%s) LIMIT %d"
    placeholders (List.length names) in
  let query = Caqti_request.collect
    ~oneshot:true typ Caqti_type.(tup2 int string) sql in
  DB.collect_list query values >>= Caqti_lwt.or_fail

let video (module DB : DBC) id =
  DB.find_opt Q.video id >>= Caqti_lwt.or_fail >>= function
  | None -> Lwt.return None
  | Some (first, second, third, fourth) ->
    let (title, slug, publish_pt) = first in
    let (expires_pt, file, md5, width) = second in
    let (height, duration, thumbnail, description) = third in
    let (cms_id, link', canonical_source_id, (created_pt, updated_pt))
      = fourth in


    let created = created_pt |> Util.int_of_ptime in
    let updated = updated_pt |> Util.int_of_ptime in
    let publish = publish_pt |> Util.int_of_ptime in
    let expires = expires_pt |> Bopt.map Util.int_of_ptime in

    let file_uri = Bopt.map Uri.of_string file in
    let filename = Uri.path (Bopt.default Uri.empty file_uri)
      |> String.split_on_char '/'
      |> BatList.last in
    let thumbnail_uri = Bopt.map Uri.of_string thumbnail in

    let%lwt tags = DB.collect_list Q.video_tags id >>= Caqti_lwt.or_fail in
    let%lwt custom = DB.collect_list Q.video_fields id
      >>= Caqti_lwt.or_fail in
    let link = Bopt.map Uri.of_string link' in

    let%lwt sources = sources_by_video_id (module DB) id in
    let canonical = sources |> List.find (fun s ->
      let id = Source.id s |> Bopt.get in
      id == canonical_source_id)
    in

    Some
      { Video. id = Some id; title; slug
      ; created; updated; publish; expires
      ; file_uri; filename; md5; width; height; duration
      ; thumbnail_uri; description; tags; custom
      ; cms_id; link; canonical; sources }
    |> Lwt.return
