
open Lwt.Infix
open Printf

module Conf = Lib.Conf


let main () =
  let conf = Conf.(JW_source.of_string @@ read_file "config.json") in

  Lwt_io.printl "Good morning, Starshine. The Earth says, \"Hello!\""
  >>= fun () ->

  let db_uri = Uri.of_string "mariadb://ovp_sync:abc123@localhost/ovp_sync" in
  Lwt.return @@ Caqti_lwt.connect_pool db_uri >>=
  Caqti_lwt.or_fail >>= fun pool ->

  let module JW = Jw_client.Platform.Make(struct 
    let key = Conf.JW_source.key conf
    let secret = Conf.JW_source.secret conf
    let rate_limit_to_leave = Conf.JW_source.rate_limit_to_leave conf
  end) in

  let module JW_var_store = Variable_store.Make(struct
    let db_pool = pool
    let namespace = "JWsrc-" ^ Conf.JW_source.key conf
  end) in

  let module Log_jw = Logger.Make(struct
    let prefix = "Src"
    let level = `Trace
  end) in

  let module JW_src = Jw_source.Make(JW)(JW_var_store)(Log_jw)(struct
    let params = ["result_limit", ["1000"]] 
    let temp_pub_tag = "Temporarily Published"
    let backup_expires_field = "ovp_sync.backup_expires_date"
  end) in

  let module Log_rdb_dest = Logger.Make(struct
    let prefix = "Dest"
    let level = `Debug
  end) in

  let module Dest = Rdb_dest.Make(Log_rdb_dest)(struct
    let db_pool = pool
    let files_path = "/Users/donut/RTM/OVPSync/data/files"
  end) in

  let module Synker = Sync.Make(JW_src)(Dest)(struct
    type src_t = JW_src.t
    type dest_t = Dest.t

    let ovp_name = "jw-" ^ (Conf.JW_source.key conf)

    let dest_t_of_src_t ((vid, file', thumb) : src_t) =
      let slug =  match BatList.assoc_opt "slug" vid.custom with
      | Some s -> s
      | None ->
        BatList.assoc_opt "file_name" vid.custom |> BatOption.default vid.title
      in

      let file, width, height = match file' with
      | None -> None, None, None 
      | Some (f, w, h) -> Some f, w, h
      in
      let file_uri = BatOption.map Uri.of_string file in
      let filename =
        BatList.assoc_opt "file_name" vid.custom |> BatOption.default slug in
      let duration_ptrn = Re.Perl.compile_pat "^(\\d+)(?:\\.(\\d{2}))?\\d*$" in
      let duration = match Re.exec_opt duration_ptrn vid.duration with
      | None -> None
      | Some g -> match Re.Group.all g with
        | [| _; ""; "" |] -> None
        | [| _;  i; "" |] -> Some (int_of_string i * 1000)
        | [| _;  i;  d |] -> Some (sprintf "%s%s0" i d |> int_of_string)
        | _ -> None
      in
      let thumbnail_uri = BatOption.map Uri.of_string thumb in

      let tags = String.split_on_char ',' vid.tags
        |> List.map String.trim
      in

      let cms_id = BatList.assoc_opt "RTM_site_ID" vid.custom 
        |> BatOption.map ((^) "rightthisminute.com-")
      in

      let module Vid_j = Jw_client.Videos_video_j in
      let author = match vid.author with
        | None -> []
        | Some a -> [ "author", a ]
      in
      let clean_j j =
        let ptrn = Re.Perl.compile_pat "^<\"|\">$" in
        Re.replace_string ~all:true ptrn ~by:"" j
      in
      let status =
        [ "status", vid.status |> Vid_j.string_of_media_status |> clean_j ] in
      let updated =
        [ "updated", vid.updated |> string_of_int ] in
      let sourcetype =
        [ "sourcetype", vid.sourcetype |> Vid_j.string_of_sourcetype
                                       |> clean_j ] in
      let mediatype =
        [ "mediatype", vid.mediatype |> Vid_j.string_of_mediatype
                                     |> clean_j ] in
      let sourceformat = match vid.sourceformat with
      | None -> []
      | Some s ->
        [ "sourceformat", s |> Vid_j.string_of_sourceformat |> clean_j ]
      in
      let size = [ "size", vid.size |> string_of_int ] in
      let md5 = match vid.md5 with
      | None -> []
      | Some m -> [ "md5", m ]
      in
      let upload_session_id = match vid.upload_session_id with
      | None -> []
      | Some u -> [ "upload_session_id", u ]
      in
      let source_custom = 
        [ author; status; updated; sourcetype; mediatype; sourceformat;
          size; md5; upload_session_id ]
        |> List.concat
      in
      let source = 
        { Rdb_dest.Source. 
          id = None
        ; name =  ovp_name
        ; media_id = vid.key
        ; video_id = None
        ; added = vid.date
        ; modified = vid.updated
        ; custom = source_custom }
      in

      { Rdb_dest.Video. 
        id = None

      ; title = vid.title
      ; slug

      ; created = vid.date
      ; updated = vid.updated
      ; publish = vid.date
      ; expires = vid.expires_date

      ; file_uri
      ; filename
      ; md5 = vid.md5
      ; width
      ; height
      ; duration

      ; thumbnail_uri
      ; description = vid.description
      ; tags = tags
      ; custom = vid.custom

      ; cms_id
      ; link = vid.link |> BatOption.map Uri.of_string

      ; canonical = source
      ; sources = [ source ] }

    let should_sync (({ key; updated; md5 }, _, _) : src_t) =
      match%lwt Dest.get_video ~ovp:ovp_name ~media_id:key with
      | None -> Lwt.return true
      | Some vid ->
        Lwt.return (
          vid.canonical.modified <> updated
          || vid.updated < updated
          || vid.md5 <> md5
        )

  end) in

  Synker.sync ()

let () = 
  Random.self_init ();
  Lwt_main.run @@ main ()
