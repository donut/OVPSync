
open Lwt.Infix
open Lib.Infix

module type DBC = Caqti_lwt.CONNECTION
module Bopt = BatOption

exception Already_inserted of string * int

module Q = struct 

  module Creq = Caqti_request
  open Caqti_type

  let video = Creq.exec
          (* title slug publish *)
    (tup4 (tup3 string string ptime)
          (* expires file_uri md5 width *)
          (tup4 (option ptime) (option string) (option string) (option int))
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
    (tup4 string string (option int) (tup2 ptime ptime))
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
    "UPDATE source SET video_id = ? WHERE id = ?"

end

let source_fields dbc source_id fields =
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
  Util.exec dbc query values

let source dbc src =
  let added_ts = Source.added src |> Util.ptime_of_int in
  let modified_ts = Source.modified src |> Util.ptime_of_int in
  let%lwt id = Util.insert dbc Q.source Source.(
    name src, media_id src, video_id src, (added_ts, modified_ts)
  ) in
  let%lwt () = source_fields dbc id (Source.custom src) in
  Lwt.return { src with id = Some id }


let new_tags_of dbc lst = 
  (* Existing names are filtered out to void incrementing the autoincrement
   * counter, as `ON DUPLICATE KEY UPDATE` does. This process is expected to
   * be run many times with mostly existing tags, since there is a reliatvely
   * small pool of tags but a large number of videos. *)
  let%lwt existing = Select.tags_by_name dbc lst in
  let names = lst |> List.filter
    (fun n -> not @@ List.exists (snd %> ((=) n)) existing) in

  if BatList.is_empty names then Lwt.return ()
  else

  let module D = Dynaparam in
  let (D.Pack (typ, values, placeholders)) = List.fold_left
    (fun pack tag -> D.add Caqti_type.string tag "(?)" pack) D.empty names in

  let sql = Printf.sprintf
    "INSERT INTO tag (name) VALUES %s ON DUPLICATE KEY UPDATE id=id"
    (String.concat ", " placeholders) in
  let query = Caqti_request.exec ~oneshot:true typ sql in

  Util.exec dbc query values


let video_tag_relations dbc video_id tag_ids =
  let module D = Dynaparam in

  let (D.Pack (typ, values, placeholders)) = List.fold_left
    (fun pack tag_id ->
      D.add Caqti_type.(tup2 int int) (video_id, tag_id) "(?, ?)" pack) 
    D.empty tag_ids in

  let sql = Printf.sprintf
    "INSERT INTO video_tag (video_id, tag_id) VALUES %s"
    (String.concat ", " placeholders) in
  let query = Caqti_request.exec ~oneshot:true typ sql in

  Util.exec dbc query values


let video_fields dbc video_id fields =
  if BatList.is_empty fields then Lwt.return ()
  else

  let module D = Dynaparam in
  let (D.Pack (typ, values, placeholders)) = List.fold_left
    (fun pk (n, v) ->
      D.add Caqti_type.(tup3 int string string) (video_id, n, v) "(?, ?, ?)" pk)
    D.empty fields in

  let placeholders = String.concat ", " placeholders in
  let sql = Printf.sprintf
    "INSERT INTO video_field (video_id, name, value) VALUES %s" placeholders in
  let query = Caqti_request.exec ~oneshot:true typ sql in

  Util.exec dbc query values

let video dbc vid =
  begin if Video.id vid |> Bopt.is_some then
    raise @@ Already_inserted ("video", Video.id vid |> Bopt.get)
  end;

  let canonical = Video.canonical vid in
  let match_canonical s = 
    Source.(name s == name canonical && media_id s == media_id canonical) in

  let%lwt sources =
    let lst = Video.sources vid in
    let includes_canonical = lst |> List.exists match_canonical in
    let lst = if includes_canonical then lst else canonical :: lst in
    lst |> Lwt_list.map_s (fun s ->
      match Source.id s with
      | Some _ -> Lwt.return s
      | None -> source dbc s
    )
  in

  let canonical = sources |> List.find match_canonical in
  
  let%lwt vid_id =
    let publish_ts = Video.publish vid |> Util.ptime_of_int in
    let expires_ts = Video.expires vid |> Bopt.map Util.ptime_of_int in
    let canonical_id = Bopt.get @@ Source.id canonical in
    let file_str = Video.file_uri vid |> Bopt.map Uri.to_string in
    let thumb_str = Video.thumbnail_uri vid |> Bopt.map Uri.to_string in
    let link_str = Video.link vid |> Bopt.map Uri.to_string in

    Util.insert dbc Q.video
      Video.( (title vid, slug vid, publish_ts)
            , (expires_ts, file_str, md5 vid, width vid)
            , (height vid, duration vid, thumb_str, description vid)
            , (cms_id vid, link_str, canonical_id) )
  in

  sources |> Lwt_list.iter_s begin fun s ->
    let id = Source.id s |> Bopt.get in
    Util.exec dbc Q.source_video_id (vid_id, id)
  end >>= fun () ->
  
  let sources =
    List.map (fun s -> Source.({ s with video_id = Some vid_id })) sources in
  let canonical = List.find match_canonical sources in
  
  new_tags_of dbc (Video.tags vid) >>= fun () ->
  let%lwt tags = Select.tags_by_name dbc (Video.tags vid) in
  let tag_ids = List.map fst tags in
  video_tag_relations dbc vid_id tag_ids >>= fun () ->
  
  video_fields dbc vid_id (Video.custom vid) >>= fun () ->

  Lwt.return { vid with id = Some vid_id; canonical; sources }