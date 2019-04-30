
module Client = Client
module Video = Video
module Source = Source

module type DBC = Caqti_lwt.CONNECTION
module Bopt = BatOption

open Lwt.Infix
open Lib.Infix

let spf = Printf.sprintf

module type Config = sig
  val db_pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t
  (* [files_path] Where to save video and thumbnail files to. *)
  val files_path : string
end

let local_scheme = "local"

let make_local_uri rel_path filename =
  let rel_path = 
    rel_path |> File.trim_slashes |> String.split_on_char '/'
    |> List.map Uri.pct_encode |> String.concat "/" in
  let filename = Uri.pct_encode filename in
  spf "%s:///%s/%s" local_scheme rel_path filename

let video_ext t = 
  let vid_id = Video.id t =?: 0 in
  let uri = Video.file_uri t =?: Uri.empty in
  ( Uri.path uri |> File.ext =?: (Video.filename t |> File.ext =?: "mov") )
  |> File.sanitize |> spf "%d.%s" vid_id

let thumb_ext t =
  let vid_id = Video.id t =?: 0 in
  let uri = Video.thumbnail_uri t =?: Uri.empty in
  let ext = (uri |> Uri.path |> File.ext =?: "jpeg")
            |> File.sanitize |> spf "%d.%s" vid_id in
  (* In case some joker used the same extension on both the image and video,
     we want to avoid file name collisions. *)
  if ext = (video_ext t) then "thumb." ^ ext else ext

let media_id_of_video t =
  Video.canonical t |> Source.media_id


module type Made = sig
  type t = Video.t
  
  (** [get_video ~ovp ~media_id] retrieves the video attached to the [ovp]  
      (Source.name) and [media_id] combination, if any. *)
  val get_video : ovp:string -> media_id:string -> t option Lwt.t
  val get_video_id_by_media_ids : (string * string) list -> int option Lwt.t

  val save : t -> t Lwt.t
end


module Make (Log : Logger.Sig) (Conf : Config) : Made = struct
  type t = Video.t


  let get_video' dbc ~ovp ~media_id =
    match%lwt Select.source dbc ~name:ovp ~media_id with
    | None -> Lwt.return None
    | Some s -> match Source.video_id s with
      | None -> Lwt.return None
      | Some id -> match%lwt Select.video dbc id with 
        | None -> Lwt.return None
        | Some t -> Lwt.return (Some t)

  let get_video ~ovp ~media_id = 
    get_video' (`Pool Conf.db_pool) ~ovp ~media_id


  let get_video_id_by_media_ids media_ids =
    Select.video_id_by_media_ids (`Pool Conf.db_pool) media_ids

  let gen_file_paths t =
    let canonical = Video.canonical t in
    let rel = Source.added canonical |> File.dir_of_timestamp in
    let abs = spf "%s/%s" Conf.files_path rel in
    let basename = Video.filename t |> File.sanitize |> File.basename in
    (abs, rel, basename)

  let abs_path_of_uri uri =
    let rel_path = 
      Uri.path uri |> File.trim_slashes
      |> String.split_on_char '/' |> List.map Uri.pct_decode
      |> String.concat "/" in
    spf "%s/%s" Conf.files_path rel_path


  let maybe_update_video_file_path dbc t new_t =
    let (_, rel_path, basename) = gen_file_paths new_t in 
    let ext = video_ext new_t in
    let filename = File.restrict_name_length basename ext in
    let new_uri = make_local_uri rel_path filename in
    let old_uri = t |> Video.file_uri |> Bopt.get |> Uri.to_string in

    if new_uri = old_uri then Lwt.return t
    else

    let media_id = media_id_of_video t in
    Log.debugf "[%s] Moving video file from [%s] to [%s]"
      media_id old_uri new_uri >>= fun () ->

    let new_abs_path = abs_path_of_uri (Uri.of_string new_uri) in
    let old_abs_path
      = abs_path_of_uri (t |> Video.file_uri |> Bopt.get) in
    Lwt_unix.rename old_abs_path new_abs_path >>= fun () ->

    let vid_id = Video.id t |> Bopt.get in
    Update.video_file_uri dbc vid_id new_uri >|= fun () ->
    { t with file_uri = Some (Uri.of_string new_uri) }


  let save_thumb_file 
    dbc ~vid_id ~media_id ~uri ~abs_path ~rel_path ~basename ~ext
  =
    let filename = File.restrict_name_length basename ext in
    let file_path = spf "%s/%s" abs_path filename in

    match%lwt File.save uri ~to_:file_path with 
    | exception exn -> 
      begin match exn with
      | File.File_error _ as exn -> raise exn
      | _ -> 
        Log.warnf ~exn "[%s] Failed saving thumbnail [%s] to [%s]"
          media_id (uri |> Uri.to_string) file_path >>= fun () ->
        File.unlink_if_exists file_path >|= fun () ->
        Error exn
      end
    | _ ->
      let local_path = make_local_uri rel_path filename in
      Update.video_thumbnail_uri dbc vid_id local_path >|= fun () ->
      Ok (Uri.of_string local_path)


  let save_video_file 
    dbc ~vid_id ~media_id ~uri ~abs_path ~rel_path ~basename ~ext
  =
    let filename = File.restrict_name_length basename ext in
    let file_path = spf "%s/%s" abs_path filename in

    match%lwt File.save uri ~to_:file_path >|= fun () -> Ok () with
    | exception exn ->
      begin match exn with
      | File.File_error _ as exn -> raise exn
      | _ ->
        Log.warnf ~exn "[%s] Failed saving video [%s] to [%s]"
          media_id (uri |> Uri.to_string) file_path >>= fun () ->
        File.unlink_if_exists file_path >|= fun () ->
        Error exn
      end
    | _ ->
      let local_path = make_local_uri rel_path filename in
      Update.video_file_uri dbc vid_id local_path >>= fun () ->
      let md5 = File.md5 file_path in
      Update.video_md5 dbc vid_id md5 >|= fun () ->
      let file_uri = Uri.of_string local_path in
      Ok (file_uri, md5)
    

  let move_temp_file temp_uri =
    let temp_path = abs_path_of_uri temp_uri in
    let suffix_ptrn = Re.Perl.compile_pat "\\.temp$" in
    let final_uri = temp_uri
      |> Uri.to_string 
      |> Re.replace_string ~all:false suffix_ptrn ~by:"" 
      |> Uri.of_string in
    let final_path = abs_path_of_uri final_uri in
    Lwt_unix.rename temp_path final_path >|= fun () ->
    final_uri

  let save_new dbc t =
    let media_id = media_id_of_video t in

    Log.debugf "[%s] Inserting into DB." media_id >>= fun () ->
    let%lwt t = Insert.video dbc t in
    let vid_id = Video.id t |> BatOption.get in
    Log.infof "[%s] Inserted as [%d]." media_id vid_id >>= fun () ->

    match Video.(file_uri t, thumbnail_uri t) with
    | None, None -> 
      Log.debugf "[%s] No file or thumbnail URIs." media_id >>= fun () ->
      Lwt.return t

    | file_uri, thumb_uri ->
      let abs_path, rel_path, basename = gen_file_paths t in
      File.prepare_dir ~prefix:Conf.files_path rel_path >>= fun _ ->

      let%lwt t = match thumb_uri with
      | None -> Lwt.return t
      | Some uri ->
        begin
          let ext = thumb_ext t in
          Log.infof "[%s] Downloading thumbnail to [%s/%s.%s]"
            media_id rel_path basename ext >>= fun () ->
          match%lwt save_thumb_file dbc ~vid_id ~media_id ~uri
                                    ~abs_path ~rel_path ~basename ~ext with
          | Error _ -> Lwt.return t
          | Ok uri -> Lwt.return { t with thumbnail_uri = Some uri }
        end
      in

      let%lwt t = match file_uri with 
      | None -> Lwt.return t
      | Some uri ->
        begin
          let ext = video_ext t in
          Log.infof "[%s] Downloading video file to [%s/%s.%s]"
            media_id rel_path basename ext >>= fun () ->
          match%lwt save_video_file dbc ~vid_id ~media_id ~uri
                                    ~abs_path ~rel_path ~basename ~ext with
          | Error _ -> Lwt.return t
          | Ok (local_uri, md5) ->
            Lwt.return { t with file_uri = Some local_uri; md5 = Some md5 }
        end
      in

      Lwt.return t

  let rm_old_file_if_renamed ~old ~new_ =
    match old with
    | Some old when old <> new_ && Uri.scheme old = Some local_scheme ->
      Log.debugf "--> Removing old file at [%s]" (Uri.to_string old)
        >>= fun () ->
      File.unlink_if_exists @@ abs_path_of_uri old
    | _ -> Lwt.return ()

  
  let update_thumbnail dbc t ~old_t ~new_t =
    let media_id = media_id_of_video t in
    let abs_path, rel_path, basename = gen_file_paths new_t in
    File.prepare_dir ~prefix:Conf.files_path rel_path >>= fun _ ->

    match Video.thumbnail_uri new_t with
    | None -> Lwt.return t
    | Some uri -> begin
      let ext = spf "%s.temp" (thumb_ext new_t) in
      Log.infof "[%s] Downloading thumbnail to [%s/%s.%s]"
        media_id rel_path basename ext >>= fun () ->

      let vid_id = Video.id t |> Bopt.get in
      let%lwt result = save_thumb_file
        dbc ~vid_id ~media_id ~uri ~abs_path ~rel_path ~basename ~ext 
      in
      match result with
      | Error _ -> Lwt.return t
      | Ok uri ->
        Log.debugf "[%s] Moving thumbnail to permanent location." media_id
          >>= fun () ->
        let%lwt uri = move_temp_file uri in
        Update.video_thumbnail_uri dbc vid_id (Uri.to_string uri) >>= fun () ->
        (* Delete old image if it was named differently, otherwise it
            would have been replaced in the move .temp move. *)
        rm_old_file_if_renamed ~old:(Video.thumbnail_uri old_t) ~new_:uri
          >|= fun () ->
        { t with thumbnail_uri = Some uri }
    end


  let update_video_file dbc t ~old_t ~new_t =
    let media_id = media_id_of_video t in
    let abs_path, rel_path, basename = gen_file_paths new_t in
    File.prepare_dir ~prefix:Conf.files_path rel_path >>= fun _ ->

    match Video.file_uri new_t with
    | None -> Lwt.return t
    | Some uri -> begin
      if Video.md5 new_t = Video.md5 old_t
        && Video.file_uri old_t |> Bopt.map Uri.scheme
            = Some (Some local_scheme)
      then maybe_update_video_file_path dbc t new_t
      else

      let ext = spf "%s.temp" (video_ext new_t) in
      Log.infof "[%s] Downloading video to [%s/%s.%s]"
        media_id rel_path basename ext >>= fun () ->
      
      let vid_id = Video.id t |> Bopt.get in
      let%lwt result = save_video_file
        dbc ~vid_id ~media_id ~uri ~abs_path ~rel_path ~basename ~ext
      in
      match result with 
      | Error _ -> Lwt.return t
      | Ok (temp_uri, md5) ->
        Log.debugf "[%s] Moving video to permanent location." media_id
          >>= fun () ->
        let%lwt uri = move_temp_file temp_uri in
        Update.video_file_uri dbc vid_id (Uri.to_string uri)
          >>= fun () ->
        rm_old_file_if_renamed ~old:(Video.file_uri old_t) ~new_:uri
          >|= fun () ->
        { t with file_uri = Some uri; md5 = Some md5 }
    end



  let save_existing dbc t_id new_t =
    let new_t = Video.{ new_t with id = Some t_id } in
    let%lwt old_t = match%lwt Select.video dbc t_id with
      | None ->
        Log.errorf "[%s] Expected existing video with ID %d, but not found."
          (media_id_of_video new_t) t_id >>= fun () ->
        raise Not_found
      | Some t -> Lwt.return t
    in

    let canonical', sources' = 
      let old_srcs = Video.sources old_t
        |> List.filter (fun os ->
          Bopt.is_none @@
            List.find_opt (Source.are_same os) (Video.sources new_t)) in
      let srcs = List.concat [old_srcs; Video.sources new_t] in
      let old_c, new_c = Video.(canonical old_t, canonical new_t) in
      let new_c = if Source.are_same old_c new_c
                  then { new_c with id = old_c.id }
                  else new_c in
      new_c, srcs
    in
    
    let t = Video.{ new_t with
      id = old_t.id;
      thumbnail_uri = old_t.thumbnail_uri;
      file_uri = old_t.file_uri;
      md5 = old_t.md5;
      canonical = canonical';
      sources = sources';
    } in

    let media_id = media_id_of_video t in
    Log.debugf "[%s] Updating fields in DB." media_id >>= fun () ->
    t |>  Update.video dbc
      >>= update_thumbnail dbc ~old_t ~new_t
      >>= update_video_file dbc ~old_t ~new_t


  let save' dbc t =
    let canonical = Video.canonical t in
    let media_id = media_id_of_video t in

    match Video.id t with
    | Some vid_id -> 
      Log.infof "[%s] Already exists as [%d]. Updating."
        media_id vid_id >>= fun () ->
      save_existing dbc vid_id t

    | None ->
      Log.debugf "[%s] Checking for existing video." media_id >>= fun () ->
      let%lwt existing = Select.source dbc
        ~name:(Source.name canonical) ~media_id:(Source.media_id canonical)
      in

      begin match existing with
      | None | Some { id = None; _ } ->
        Log.debugf "[%s] Saving as new video." media_id >>= fun () ->
        save_new dbc t
      | Some { id = Some src_id; video_id = None; _ } ->
        (* Looks like the process was stopped after the source was created, 
          but before the video's inserted ID was saved to the source. Better
          to just clear the slate and treat as new instead of handling all 
          partial save edge cases. No files should have been saved, and the DB
          relations should be such that deleting the source will also delete all
          rows in other tables that reference it. *)
        Log.debugf "[%s] Source already exists as [%d]. Deleting before saving as new video."
          media_id src_id >>= fun () ->
        Delete.source dbc src_id >>= fun () ->
        Lwt_unix.sleep 10. >>= fun () ->
        save_new dbc t
      | Some { video_id = Some vid_id; _ } ->
        Log.infof "[%s] Already exists as [%d]. Updating."
          media_id vid_id >>= fun () ->
        save_existing dbc vid_id t
      end

  
  let save t = 
    let media_id = media_id_of_video t in
    Log.debugf "Saving [%s]." media_id >>= fun () ->
    
    match%lwt save' (`Pool Conf.db_pool) t with
    | exception exn ->
      Log.errorf ~exn "[%s] Failed saving." (media_id_of_video t) >>= fun () ->
      raise exn
    | t ->
      Log.debugf "[%s] Finished saving." media_id >|= fun () ->
      t

end