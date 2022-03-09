
open Lib.Infix.Function
open Lib.Infix.Option

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
    "SELECT id, name, media_id, video_id, added, modified \
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
    int (tup2 int string)
    "SELECT tag.id, tag.name FROM video_tag \
       JOIN tag ON tag.id = video_tag.tag_id \
      WHERE video_tag.video_id = ? \
      ORDER BY tag.name"

end


let source_of_row row ~custom =
  let ((id, name, media_id, video_id), added_pt, modified_pt) = row in
  let added = added_pt |> Util.int_of_ptime in
  let modified = modified_pt |> Util.int_of_ptime in
  { Source. id = Some id; name; media_id; video_id; custom; added; modified }

let source_fields dbc source_id =
  Util.collect_list dbc Q.source_fields source_id

let source dbc ~name ~media_id =
  match%lwt Util.find_opt dbc Q.source (name, media_id) with
  | None -> Lwt.return None
  | Some row ->
    let id = BatTuple.(Tuple3.first row |> Tuple4.first) in
    let%lwt custom = source_fields dbc id in
    Lwt.return @@ Some (source_of_row row ~custom)

let source_id dbc ~name ~media_id =
  Util.find_opt dbc Q.source_id (name, media_id)

let source_fields_by_video_id dbc video_id =
  let%lwt fields =
    Util.collect_list dbc Q.source_fields_by_video_id video_id 
  in
  fields
    |> BatList.group (fun (a, _, _) (b, _, _) -> compare a b)
    |> List.map (fun l ->
      let source_id = List.hd l |> BatTuple.Tuple3.first in
      let l' = List.map (fun (_, n, v) -> (n, v)) l in
      (source_id, l'))
    |> Lwt.return

let sources_by_video_id dbc video_id =
  let%lwt sources = Util.collect_list dbc Q.sources_by_video_id video_id in
  let%lwt fields = source_fields_by_video_id dbc video_id in
  sources
    |> List.map (fun row ->
      let id = BatTuple.(Tuple3.first row |> Tuple4.first) in
      let custom = List.assoc_opt id fields |> Bopt.default [] in
      source_of_row row ~custom)
    |> Lwt.return


let tags_by_name dbc names = 
  if BatList.is_empty names then Lwt.return []
  else

  let module D = Dynaparam in

  let D.Pack (typ, values, plist) = List.fold_left
    (fun pack name -> D.add Caqti_type.string name "?" pack)
    D.empty names in
  let placeholders = String.concat ", " plist in

  let sql = Printf.sprintf 
    "SELECT id, name FROM tag WHERE name IN (%s) LIMIT %d"
    placeholders (List.length names) in
  let query = Caqti_request.collect
    ~oneshot:true typ Caqti_type.(tup2 int string) sql in

  Util.collect_list dbc query values


let tags_by_video_id dbc vid_id =
  Util.collect_list dbc Q.video_tags vid_id


exception Wrong_canonical_source of { video_id : int }


let video dbc id =
  match%lwt Util.find_opt dbc Q.video id with
  | None -> Lwt.return None
  | Some (first, second, third, fourth) ->
    let title, slug, publish_pt = first in
    let expires_pt, file, md5, width = second in
    let height, duration, thumbnail, description = third in
    let cms_id, link', canonical_source_id, (created_pt, updated_pt) = fourth in

    let created = created_pt |> Util.int_of_ptime in
    let updated = updated_pt |> Util.int_of_ptime in
    let publish = publish_pt |> Util.int_of_ptime in
    let expires = expires_pt |> Bopt.map Util.int_of_ptime in

    let file_uri = Bopt.map Uri.of_string file in
    let filename =
      file_uri
      |> Bopt.default Uri.empty
      |> Uri.path
      |> String.split_on_char '/'
      |> BatList.last
      |> Uri.pct_decode in
    let thumbnail_uri = Bopt.map Uri.of_string thumbnail in

    let%lwt tags = 
      let%lwt pairs = tags_by_video_id dbc id in
      List.map snd pairs |> Lwt.return
    in
    let%lwt custom = Util.collect_list dbc Q.video_fields id in
    let link = link' >|? Uri.of_string in

    let%lwt sources = sources_by_video_id dbc id in
    let canonical = 
      sources
      |> List.find_opt (Source.id %> (=) (Some canonical_source_id))
      |> function 
        | None -> 
          raise @@ Wrong_canonical_source { video_id = id }
        | Some source ->
          source
    in

    Some
      { Video. id = Some id; title; slug
      ; created; updated; publish; expires
      ; file_uri; filename; md5; size = None; width; height; duration
      ; thumbnail_uri; description; tags; custom
      ; cms_id; link; canonical; sources }
    |> Lwt.return


let video_id_by_media_ids dbc media_ids = 
  let module D = Dynaparam in
  let placeholder = "(name = ? AND media_id = ?)" in
  let typ = Caqti_type.(tup2 string string) in
  let (D.Pack (typs, values, plist)) = List.fold_left
    (fun pack m -> D.add typ m placeholder pack)
    D.empty media_ids in
  let placeholders = String.concat " OR " plist in
  let sql = Printf.sprintf 
    "SELECT video_id FROM source \
      WHERE video_id IS NOT NULL \
        AND ( %s ) \
      LIMIT 1"
    placeholders in
  let query = Caqti_request.find_opt ~oneshot:true typs Caqti_type.int sql in
  Util.find_opt dbc query values