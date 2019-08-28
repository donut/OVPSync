

open Lib.Infix
open Printf

module Blist = BatList
module Bopt = BatOption
module Bstr = BatString

module Vid_j = Jw_client.Videos_video_j


type video = Jw_client.Platform.videos_list_video


let get_slug (vid : video) = 
  Blist.assoc_opt "slug" vid.custom
    =?: (Blist.assoc_opt "file_name" vid.custom =?: vid.title)


let get_file_details = function 
  | None -> None, None, None
  | Some (f, w, h) ->
    let uri = Uri.of_string f in
    Some uri, w, h


let get_filename (vid : video) ~fallback =
  let name = Blist.assoc_opt "file_name" vid.custom =?: fallback in
  (* Some names are just quotes, which isn't really desired. *)
  let trim_quotes =
    Re.replace_string (Re.Perl.compile_pat "^[\"']+$") ~by:"" in
  if String.length (trim_quotes name) = 0 then vid.title else name 


let parse_duration d =
  let duration_ptrn = Re.Perl.compile_pat "^(\\d+)(?:\\.(\\d{2}))?\\d*$" in
  match Re.exec_opt duration_ptrn d with
  | None -> None
  | Some g -> match Re.Group.all g with
    | [| _; ""; "" |] -> None
    | [| _;  i; "" |] -> Some (int_of_string i * 1000)
    | [| _;  i;  d |] -> Some (sprintf "%s%s0" i d |> int_of_string)
    | _ -> None


let pair_of_field name v =
  [name, v]
let pair_of_opt_field name v =
  Bopt.map_default (pair_of_field name) [] v
let pair_of_jfield name f v =
  let clean_j j =
    let ptrn = Re.Perl.compile_pat "^<\"|\">$" in
    Re.replace_string ~all:true ptrn ~by:"" j
  in
  pair_of_field name (f v |> clean_j)
let pair_of_opt_jfield name f v =
  Bopt.map_default (pair_of_jfield name f) [] v

let get_author (v : video) = pair_of_opt_field "author" v.author
let get_status (v : video) = 
  pair_of_jfield "status" Vid_j.string_of_media_status v.status 
let get_sourcetype (v : video) =
  pair_of_jfield "sourcetype" Vid_j.string_of_sourcetype v.sourcetype 
let get_mediatype (v : video) = 
  pair_of_jfield "mediatype" Vid_j.string_of_mediatype v.mediatype 
let get_sourceformat (v : video) =
  pair_of_opt_jfield "sourceformat" Vid_j.string_of_sourceformat v.sourceformat 
let get_md5 (v : video) = pair_of_opt_field "md5" v.md5
let get_upload_session_id (v : video) = 
  pair_of_opt_field "upload_session_id" v.upload_session_id


let get_source_custom (vid : video) = 
  let author = get_author vid in
  let status = get_status vid in
  let sourcetype = get_sourcetype vid in
  let mediatype = get_mediatype vid in
  let sourceformat = get_sourceformat vid in
  let md5 = get_md5 vid in
  let upload_session_id = get_upload_session_id vid in
  (* [size] is left out since it is based on the total size of all 
      conversions. This is not important metadata and changes without
      anything else changing. Also, it changes when adding the passthrough
      conversion which is necessary to sync, which means when it tries to
      check if a sync is needed, it sees the size difference since the
      passthrough conversion is removed after sync. *)
  [ author; status; sourcetype; mediatype; sourceformat;
    md5; upload_session_id ]
  |> List.concat


let get_source (vid : video) ~ovp_name =
  { Rdb_dest.Source. 
    id = None
  ; name =  ovp_name
  ; media_id = vid.key
  ; video_id = None
  ; added = vid.date
  ; modified = vid.updated
  ; custom = get_source_custom vid }


let get_existing_video_id (v : video) ~ovp_name finder =
  let media_ids = ref [||] in
  let append k v = media_ids := Array.append !media_ids [| k, v |] in
  append ovp_name v.key;
  List.assoc_opt "Ooyala_content_ID" v.custom >|? (append "ooyala")
    |> ignore;
  List.assoc_opt "OVPMigrate_ID" v.custom >|? (append "ovp_migrate")
    |> ignore;
  finder (!media_ids |> Array.to_list)


let get_custom (v : video) =
  (* Make custom field names case insensitively unqiue. JW uses case 
     sensitive comparisons, but the video_field.value column in the DB
     uses an insensitive collation. Thinking about future compatibility,
     and possibily working with other systems, it seems like it'd be better
     to store the values in a way that is most likely to be compatible. *)
  let rec uniquify ?(num=0) ?base keys key =
    let low_k = Bstr.lowercase key in
    if List.exists ((=) low_k) keys then
      let base = base =?: key in
      let k = sprintf "%s_%d" base num in
      uniquify ~num:(num + 1) ~base keys k
    else
      key
  in
  v.custom |> List.fold_left begin fun (keys, lst) (k, v) ->
    let key = uniquify keys k in
    let low_k = Bstr.lowercase key in
    (low_k :: keys, (key, v) :: lst)
  end ([], []) |> snd


let get_cms_id (v : video) =
  Blist.assoc_opt "RTM_site_ID" v.custom >|? ((^) "rightthisminute.com-")


let make
  ~(conf : Lib.Conf.Sync.t)
  ~ovp_name
  (module Src : Jw_source.Made) 
  (module Dest : Rdb_dest.Made)
  (module Log : Logger.Sig)
=
  let module Synker = Sync.Make(Src)(Dest)(Log)(struct
    type src_t = Src.t
    type dest_t = Dest.t

    let max_threads = conf.max_threads
    let loop_infinitely = conf.loop_infinitely

    let dest_t_of_src_t ((vid, file', thumb) : src_t) =
      let%lwt id =
        get_existing_video_id vid ~ovp_name Dest.get_video_id_by_media_ids in
      let slug = get_slug vid in
      let file_uri, width, height = get_file_details file' in
      let source = get_source vid ~ovp_name in

      { Rdb_dest.Video. 
        id
      ; title = vid.title
      ; slug

      ; created = vid.date
      ; updated = vid.updated
      ; publish = vid.date
      ; expires = vid.expires_date

      ; file_uri
      ; filename = get_filename vid ~fallback:slug
      ; md5 = vid.md5
      ; width
      ; height
      ; duration = parse_duration vid.duration

      ; thumbnail_uri = thumb >|? Uri.of_string
      ; description = vid.description
      ; tags = String.(split_on_char ',' vid.tags |> List.map trim)
      ; custom = get_custom vid

      ; cms_id = get_cms_id vid
      ; link = vid.link >|? Uri.of_string

      ; canonical = source
      ; sources = [ source ] }
      |> Lwt.return


    let should_sync src_item =
      let ({ key; sourcetype; _ }, _, _) : src_t = src_item in

      match%lwt Dest.get_video ~ovp:ovp_name ~media_id:key with
      | None -> Lwt.return true

      | Some old ->
        let%lwt knew = dest_t_of_src_t src_item in
        let debug = Log.level = `Trace in
        let check_md5 = sourcetype = `File in
        Lwt.return @@ Has_changed.video ~debug ~check_md5 old knew

  end) in

  (module Synker : Sync.Synchronizer)