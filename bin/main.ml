
open Lwt.Infix
open Lib.Infix
open Printf

module Conf = Lib.Conf
module Blist = BatList
module Bopt = BatOption
module Bstr = BatString

exception Caqti_conn_error of string
exception Unexpected_arguments of string * string

let main () =
  let conf =
    let path = match Sys.argv with
      | [|_; path|] -> path
      | _ ->
        let args = Sys.argv |> Array.to_list |> String.concat "; " in
        raise @@ Unexpected_arguments
          ("The config path must be the only argument.", args)
    in
    Conf.read_config path
  in

  let get_log_level = Conf.log_level conf in

  Lwt_io.printl "Good morning, Starshine. The Earth says, \"Hello!\""
  >>= fun () ->

  let db_uri = Uri.of_string conf.db_conn in
  let pool = match Caqti_lwt.connect_pool db_uri with
  | Ok p -> p
  | Error e -> raise @@ Caqti_conn_error (Caqti_error.show e)
  in

  let module Log_jw_client = Logger.Make(struct
    let prefix = conf.jw_client.log_namespace
    let level = get_log_level conf.jw_client.log_level
  end) in

  let module JW = Jw_client.Platform.Make(Log_jw_client)(struct 
    let key = conf.jw_client.key
    let secret = conf.jw_client.secret
    let rate_limit_to_leave = conf.jw_client.rate_limit_to_leave
  end) in

  let module JW_var_store = Variable_store.Make(struct
    let db_pool = pool
    let namespace = "JWsrc-" ^ conf.jw_client.key
  end) in

  let module Log_jw_src = Logger.Make(struct
    let prefix = conf.jw_source.log_namespace
    let level = get_log_level conf.jw_source.log_level
  end) in

  let module JW_src = Jw_source.Make(JW)(JW_var_store)(Log_jw_src)(struct
    let chunk_size = conf.jw_source.chunk_size |> string_of_int
    
    let params = ["result_limit", [chunk_size]] 
    let temp_pub_tag = conf.jw_source.temp_publish_tag
    (* let backup_expires_field =  *)
    let backup_expires_field = conf.jw_source.backup_expires_field
  end) in

  let module Log_rdb_dest = Logger.Make(struct
    let prefix = conf.rdb_dest.log_namespace
    let level = get_log_level conf.rdb_dest.log_level
  end) in

  let module Dest = Rdb_dest.Make(Log_rdb_dest)(struct
    let db_pool = pool
    let files_path = conf.rdb_dest.files_path
  end) in

  let module Log_synker = Logger.Make(struct
    let prefix = conf.rdb_dest.log_namespace
    let level = get_log_level conf.sync.log_level
  end) in

  let module Synker = Sync.Make(JW_src)(Dest)(Log_synker)(struct
    type src_t = JW_src.t
    type dest_t = Dest.t

    let max_threads = conf.sync.max_threads
    let loop_infinitely = conf.sync.loop_infinitely

    let ovp_name = "jw-" ^ conf.jw_client.key

    let dest_t_of_src_t ((vid, file', thumb) : src_t) =
      let slug = match Blist.assoc_opt "slug" vid.custom with
      | Some s -> s
      | None ->
        Blist.assoc_opt "file_name" vid.custom |> BatOption.default vid.title
      in

      let file, width, height = match file' with
      | None -> None, None, None 
      | Some (f, w, h) -> Some f, w, h
      in
      let file_uri = file >|? Uri.of_string in
      let filename =
        let name = Blist.assoc_opt "file_name" vid.custom =?: slug in
        (* Some names are just quotes, which isn't really desired. *)
        let trim_quotes =
          Re.replace_string (Re.Perl.compile_pat "^[\"']+$") ~by:"" in
        if String.length (trim_quotes name) = 0 then vid.title else name 
      in
      let duration_ptrn = Re.Perl.compile_pat "^(\\d+)(?:\\.(\\d{2}))?\\d*$" in
      let duration = match Re.exec_opt duration_ptrn vid.duration with
      | None -> None
      | Some g -> match Re.Group.all g with
        | [| _; ""; "" |] -> None
        | [| _;  i; "" |] -> Some (int_of_string i * 1000)
        | [| _;  i;  d |] -> Some (sprintf "%s%s0" i d |> int_of_string)
        | _ -> None
      in
      let thumbnail_uri = thumb >|? Uri.of_string in

      let tags = String.split_on_char ',' vid.tags |> List.map String.trim in

      let cms_id = Blist.assoc_opt "RTM_site_ID" vid.custom 
                   >|? ((^) "rightthisminute.com-")
      in

      let source = 
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
        let md5 = match vid.md5 with
          | None -> []
          | Some m -> [ "md5", m ]
        in
        let upload_session_id = match vid.upload_session_id with
          | None -> []
          | Some u -> [ "upload_session_id", u ]
        in
        (* [size] is left out since it is based on the total size of all 
           conversions. This is not important metadata and changes without
           anything else changing. Also, it changes when adding the passthrough
           conversion which is necessary to sync, which means when it tries to
           check if a sync is needed, it sees the size difference since the
           passthrough conversion is removed after sync. *)
        let source_custom = 
          [ author; status; sourcetype; mediatype; sourceformat;
            md5; upload_session_id ]
          |> List.concat
        in

        { Rdb_dest.Source. 
          id = None
        ; name =  ovp_name
        ; media_id = vid.key
        ; video_id = None
        ; added = vid.date
        ; modified = vid.updated
        ; custom = source_custom }
      in

      let%lwt id = 
        let media_ids = ref [||] in
        let append k v = media_ids := Array.append !media_ids [| k, v |] in
        append ovp_name vid.key;
        List.assoc_opt "Ooyala_content_ID" vid.custom >|? (append "ooyala")
          |> ignore;
        List.assoc_opt "OVPMigrate_ID" vid.custom >|? (append "ovp_migrate")
          |> ignore;
        Dest.get_video_id_by_media_ids (!media_ids |> Array.to_list)
      in

      (* Make custom field names case insensitively unqiue. JW uses case 
         sensitive comparisons, but the video_field.value column in the DB
         uses an insensitive collation. Thinking about future compatibility,
         and possibily working with other systems, it seems like it'd be better
         to store the values in a way that is most likely to be compatible. *)
      let custom = 
        let rec uniquify ?(num=0) ?base keys key =
          let low_k = Bstr.lowercase key in
          if List.exists ((=) low_k) keys then
            let base = base =?: key in
            let k = sprintf "%s_%d" base num in
            uniquify ~num:(num + 1) ~base keys k
          else
            key
        in
        vid.custom |> List.fold_left begin fun (keys, lst) (k, v) ->
          let key = uniquify keys k in
          let low_k = Bstr.lowercase key in
          (low_k :: keys, (key, v) :: lst)
        end ([], []) |> snd
      in

      { Rdb_dest.Video. 
        id
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
      ; tags
      ; custom

      ; cms_id
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
        let check_md5 = sourcetype = `File in
        Lwt.return @@ Rdb_dest.Video.has_changed ~check_md5 old knew

  end) in

  Synker.sync ()

let () = 
  Random.self_init ();
  Lwt_main.run @@ main ()
