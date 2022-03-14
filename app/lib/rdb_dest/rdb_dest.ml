
module Client = Client
module Video = Video
module Source = Source

module type DBC = Caqti_lwt.CONNECTION
module Bopt = BatOption

open Lwt.Infix
open Lib.Infix.Option


let spf = Printf.sprintf
let estimated_safe_max_image_size = 10_485_760
(* A size in bytes that is realistically far outside the likely values for
   images. This is used to estimate how much space we should expect an image
   to use when selecting the file store to use. *)

let is_local_uri = File_store.is_local_uri


module type Config = sig
  val db_pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t

  val file_stores : string list
  (* List of absolute path prefixes for possible locations to store files. *)
end


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

let make_relative_path video =
  let canonical = Video.canonical video in
  let rel = Source.added canonical |> File.dir_of_timestamp in
  let basename = Video.filename video |> File.sanitize |> File.basename in
  (rel, basename)


  (** [maybe_update_video_file_path dbc store t new_t] renames/moves existing  
      video file referenced by [t.file_uri] if changes in [new_t] warrant them. 
      For example, slug or creation date changes. *)
  let maybe_update_video_file_path dbc store t new_t =
    let rel_path, basename = make_relative_path new_t in 
    let ext = video_ext new_t in
    let filename = File.restrict_name_length basename ext in
    let new_uri_string = File_store.make_local_uri rel_path filename in
    let old_uri_string = t |> Video.file_uri |> Bopt.get |> Uri.to_string in

    if new_uri_string = old_uri_string 
    then Lwt.return t
    else

    let media_id = media_id_of_video t in
    Log.debugf "[%s] Moving video file from [%s] to [%s]"
      media_id old_uri_string new_uri_string >>= fun () ->

    let old_uri = old_uri_string |> Uri.of_string in
    let new_uri = new_uri_string |> Uri.of_string in

    let old_abs_path = old_uri |> File_store.abs_path_of_local_uri store in
    let new_abs_path = new_uri |> File_store.abs_path_of_local_uri store in
    Lwt_unix.rename old_abs_path new_abs_path >>= fun () ->

    let vid_id = Video.id t |> Bopt.get in
    let%lwt () = Update.video_file_uri dbc vid_id (Some new_uri_string) in
    Lwt.return { t with file_uri = Some new_uri }


  let save_thumb_file 
    dbc ~store ~uri ~vid_id ~media_id ~rel_path ~basename ~ext
  =
    let filename = File.restrict_name_length basename ext in
    let abs_path = spf "%s/%s" store rel_path in
    let file_path = spf "%s/%s" abs_path filename in

    match%lwt File.save uri ~to_:file_path with 
    | exception exn -> 
      begin match exn with
      | File.File_error _ as exn -> 
        raise exn
      | _ -> 
        Log.warnf "[%s] Failed saving thumbnail [%s] to [%s]"
          media_id (uri |> Uri.to_string) file_path >>= fun () ->
        File.unlink_if_exists file_path >|= fun () ->
        Error exn
      end
    | _ ->
      let local_path = File_store.make_local_uri rel_path filename in
      let%lwt () = Update.video_thumbnail_uri dbc vid_id (Some local_path) in
      Lwt.return @@ Ok (Uri.of_string local_path)


  let save_video_file 
    dbc ~store ~uri ~vid_id ~media_id ~rel_path ~basename ~ext
  =
    let filename = File.restrict_name_length basename ext in
    let abs_path = spf "%s/%s" store rel_path in
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
      let local_path = File_store.make_local_uri rel_path filename in
      Update.video_file_uri dbc vid_id (Some local_path) >>= fun () ->
      let md5 = File.md5 file_path in
      Update.video_md5 dbc vid_id md5 >|= fun () ->
      let file_uri = Uri.of_string local_path in
      Ok (file_uri, md5)
    

  let make_temp_file_permanent ~store temp_uri =
    let suffix_ptrn = Re.Perl.compile_pat "\\.temp$" in

    let temp_path = temp_uri |> File_store.abs_path_of_local_uri store in
    let final_uri = temp_uri
      |> Uri.to_string 
      |> Re.replace_string ~all:false suffix_ptrn ~by:"" 
      |> Uri.of_string in
    let final_path = final_uri |> File_store.abs_path_of_local_uri store in

    Lwt_unix.rename temp_path final_path >|= fun () ->
    final_uri


  (** [required_space_of_t t] makes an estimate of how much space will be
      required to save its files (video and thumbnail). *)
  let required_space_of_t t =
    let fallback thumb_size t = t |> Video.size >|? (+) thumb_size in

    let%lwt thumb_size = 
      begin match t |> Video.thumbnail_uri with
      | None -> 
        Lwt.return None
      | Some uri -> 
        let%lwt size = uri |> File.content_length_of_uri in
        Lwt.return (size >|? Int64.to_int)
      end
      >|= Bopt.default estimated_safe_max_image_size
    in

    match t |> Video.file_uri with
    | None -> 
      fallback thumb_size t |> Lwt.return
    
    | Some vid_uri ->
      begin match%lwt vid_uri |> File.content_length_of_uri with
      | None -> 
        fallback thumb_size t |> Lwt.return
      | Some vid_size -> 
        let vid_size = vid_size |> Int64.to_int in
        Some (vid_size + thumb_size) |> Lwt.return
      end


  (** [unwrap_and_report_on_picked_store_exn media_id store_pick] returns the 
      store path in [store_pick] if it's not {!None}. Otherwise, logs and 
      raises on error or no store found with enough space. *)
  let unwrap_and_report_on_picked_store_exn media_id store_pick =
    match store_pick with
    | Error (level, exn, msg) ->
      let%lwt () = Log.logf level ?exn "[%s] %s" media_id msg in
      begin match exn with
      | None -> failwith msg
      | Some exn -> raise exn
      end

    | Ok { File_store. path=None; required_space; checked; _  } ->
      let checked = 
        checked 
        |> List.map (fun (store, space) -> spf "[%s: %i B]" store space)
        |> String.concat " " in
      let msg = spf 
        "[%s] No file stores with required space: %i B. Checked stores: %s"
        media_id required_space checked in
      let%lwt () = Log.fatal msg in
      failwith msg

    | Ok { File_store. path=(Some path); available_space; required_space; _ } ->
      let%lwt _ = Log.debugf 
        "[%s] Picked file store [%s: %i B] with estimated usage of %i B"
        media_id path available_space required_space in
      Lwt.return path


  let save_new dbc t =
    let media_id = media_id_of_video t in

    (* Save to DB. *)
    Log.debugf "[%s] Inserting into DB." media_id >>= fun () ->
    let%lwt t = Insert.video dbc t in

    let vid_id = Video.id t |> BatOption.get in
    Log.infof "[%s] Inserted as [%d]." media_id vid_id >>= fun () ->

    (* Save files. *)
    match Video.(file_uri t, thumbnail_uri t) with
    | None, None -> 
      Log.debugf "[%s] No file or thumbnail URIs." media_id >>= fun () ->
      Lwt.return t

    | file_uri, thumb_uri ->
      let%lwt store = 
        let%lwt required_space = required_space_of_t t in

        Conf.file_stores
        |> File_store.pick ?required_space
        |> unwrap_and_report_on_picked_store_exn media_id in

      let rel_path, basename = make_relative_path t in
      File.prepare_dir ~prefix:store rel_path >>= fun _ ->

      let%lwt t = 
        match thumb_uri with
        | None -> 
          Lwt.return t

        | Some uri -> begin
          let ext = thumb_ext t in

          Log.infof "[%s] Downloading thumbnail to [%s/%s.%s]"
            media_id rel_path basename ext >>= fun () ->

          match%lwt 
            save_thumb_file 
              dbc ~store ~uri ~vid_id ~media_id ~rel_path ~basename ~ext 
          with
          | Error _ -> Lwt.return t
          | Ok uri -> Lwt.return { t with thumbnail_uri = Some uri }
        end
      in

      let%lwt t = 
        match file_uri with 
        | None -> 
            Lwt.return t

        | Some uri -> begin
          let ext = video_ext t in
          Log.infof "[%s] Downloading video file to [%s/%s.%s]"
            media_id rel_path basename ext >>= fun () ->

          match%lwt 
            save_video_file 
              dbc ~store ~uri ~vid_id ~media_id ~rel_path ~basename ~ext 
          with
          | Error _ -> Lwt.return t
          | Ok (local_uri, md5) ->
            Lwt.return { t with file_uri = Some local_uri; md5 = Some md5 }
        end
      in

      Lwt.return t


  let unlink_old_file_if_renamed ~old ~new_ =
    match old with
    | Some old when old |> File_store.is_local_uri ->
      begin if old <> new_ then
        let%lwt () = Log.debugf 
          "--> Removing old file at [%s]" (Uri.to_string old) in
        File_store.unlink_uri_if_found_on_stores Conf.file_stores old

      else
        let%lwt old_store = 
          old |> File_store.store_of_local_uri Conf.file_stores in
        let%lwt new_store = 
          new_ |> File_store.store_of_local_uri Conf.file_stores in

        match old_store, new_store with
        | None, _ (* Nothing we have access to to delete. *)
        |    _, None -> (* Renamed to nothing? *)
          Lwt.return ()

        | Some old_store, Some new_store when old_store = new_store ->
          Lwt.return ()

        | Some old_store, Some _ ->
          old
          |> File_store.abs_path_of_local_uri old_store
          |> File.unlink_if_exists
      end

    | _ -> 
      Lwt.return ()

  
  let update_thumbnail dbc ~store ~old_t ~new_t t =
    let vid_id = Video.id t |> Bopt.get in

    match new_t |> Video.thumbnail_uri with
    | None ->
      (* Keep the old URI in place if the thumbnail image it links to was
         already saved to the store. It could be that the thumbnail no longer
         makes sense for this video, but we don't lose anything for keeping it.
         *)
      begin match old_t |> Video.thumbnail_uri with
      | Some uri when not (File_store.is_local_uri uri) -> 
        let%lwt () = Update.video_thumbnail_uri dbc vid_id None in
        Lwt.return { t with thumbnail_uri = None }
        
      | _ -> 
        Lwt.return t
      end

    | Some uri -> begin
      let media_id = media_id_of_video t in
      let rel_path, basename = new_t |> make_relative_path in
      let%lwt _ = File.prepare_dir ~prefix:store rel_path in

      let ext = spf "%s.temp" (thumb_ext new_t) in
      let%lwt () = Log.infof
        "[%s] Downloading thumbnail to [%s/%s.%s]"
        media_id rel_path basename ext in

      match%lwt
        save_thumb_file
          dbc ~store ~uri ~vid_id ~media_id ~rel_path ~basename ~ext
      with
      | Error _ -> 
        Lwt.return t

      | Ok uri ->
        let%lwt () = Log.debugf
          "[%s] Moving thumbnail to permanent location." media_id in
        let%lwt uri = make_temp_file_permanent ~store uri in
        
        let%lwt () =
          let str = Uri.to_string uri in
          Update.video_thumbnail_uri dbc vid_id (Some str)
        in

        (* Delete old image if it was named differently or saved to a different 
           store, otherwise it would have been replaced in the move .temp move. 
           *)
        unlink_old_file_if_renamed ~old:(Video.thumbnail_uri old_t) ~new_:uri 
          >>= fun () ->

        Lwt.return { t with thumbnail_uri = Some uri }
    end


  let update_video_file dbc ~store ~old_t ~new_t t =
    let vid_id = Video.id t |> Bopt.get in

    match Video.file_uri new_t with
    | None ->
      (* Keep the old URI in place if the video it links to was already saved 
         to the store. We lose nothing by keeping the old file around. *)
      begin match Video.file_uri old_t with
      | Some uri when not (uri |> File_store.is_local_uri) -> 
        let%lwt () = Update.video_file_uri dbc vid_id None in
        Lwt.return { t with file_uri = None }
      | _ -> 
        Lwt.return t
      end

    | Some uri -> begin
      let media_id = media_id_of_video t in
      let rel_path, basename = make_relative_path new_t in
      File.prepare_dir ~prefix:store rel_path >>= fun _ ->

      let old_uri_is_local = 
        old_t
        |> Video.file_uri
        >|? File_store.is_local_uri
        =?: false in

      if Video.md5 new_t = Video.md5 old_t && old_uri_is_local
      then maybe_update_video_file_path dbc store t new_t
      else

      let ext = spf "%s.temp" (video_ext new_t) in
      Log.infof "[%s] Downloading video to [%s/%s.%s]"
        media_id rel_path basename ext >>= fun () ->
      
      match%lwt
        save_video_file
          dbc ~store ~uri ~vid_id ~media_id ~rel_path ~basename ~ext
      with 
      | Error _ -> 
        Lwt.return t

      | Ok (temp_uri, md5) ->
        let%lwt () = 
          Log.debugf "[%s] Moving video to permanent location." media_id in
        let%lwt uri = make_temp_file_permanent ~store temp_uri in

        let%lwt () =
          let str = uri |> Uri.to_string in
          Update.video_file_uri dbc vid_id (Some str)
        in

        unlink_old_file_if_renamed ~old:(Video.file_uri old_t) ~new_:uri
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

      | Some t -> 
        Lwt.return t
    in

    let canonical', sources' = 
      let old_srcs = 
        Video.sources old_t
        |> List.filter begin fun os ->
          List.find_opt (Source.are_same os) (Video.sources new_t)
          |> Bopt.is_none
        end in

      let srcs = List.concat [old_srcs; Video.sources new_t] in
      let old_c, new_c = Video.(canonical old_t, canonical new_t) in
      let new_c = if Source.are_same old_c new_c
                  then { new_c with id = old_c.id }
                  else new_c in

      new_c, srcs
    in
    
    let t = Video.
      { new_t with
        id = old_t.id
      ; thumbnail_uri = old_t.thumbnail_uri
      ; file_uri = old_t.file_uri
      ; md5 = old_t.md5
      ; canonical = canonical'
      ; sources = sources' } in

    let media_id = media_id_of_video t in
    Log.debugf "[%s] Updating fields in DB." media_id >>= fun () ->
    
    let%lwt t = t |> Update.video dbc in

    (* If possible, we'd like to keep the files on the same store they're on
       now. This helps prevent fragmentation (how many videos from around the 
       same times are split between stores. *)
    let%lwt store =
      (* Figure out which store(s) the old files are currently on. *)
      let%lwt old_video = 
        match t |> Video.file_uri with
        | None -> Lwt.return None
        | Some uri -> uri |> File_store.File.of_local_uri Conf.file_stores in
      let%lwt old_thumb =
        match t |> Video.thumbnail_uri with
        | None -> Lwt.return None
        | Some uri -> uri |> File_store.File.of_local_uri Conf.file_stores in
      
      let old_store = 
        let open File_store.File in
        match old_video, old_thumb with
        | None, None -> None
        | None, Some { store; _ } -> Some store
        | Some { store; _ }, _ -> Some store 
      in
      
      let%lwt store_pick = 
        let%lwt required_space = required_space_of_t new_t in
        let stores =
          match old_store with
          | None -> Conf.file_stores
          | Some old -> old :: Conf.file_stores in
        stores |> File_store.pick ?required_space |> Lwt.return
      in

      store_pick |> unwrap_and_report_on_picked_store_exn media_id
    in
   
    t
    |> update_thumbnail dbc ~store ~old_t ~new_t 
    >>= update_video_file dbc ~store ~old_t ~new_t 


  let save' dbc t =
    let canonical = Video.canonical t in
    let media_id = media_id_of_video t in

    match Video.id t with
    | Some vid_id -> 
      Log.infof "[%s] Already exists as [%d]. Updating..."
        media_id vid_id >>= fun () ->
      save_existing dbc vid_id t

    | None ->
      Log.debugf "[%s] Checking for existing video." media_id >>= fun () ->
      let%lwt existing = Select.source dbc
        ~name:(Source.name canonical) ~media_id:(Source.media_id canonical) in

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
        Log.debugf 
          "[%s] Source already exists as [%d]. Deleting before saving as new video."
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