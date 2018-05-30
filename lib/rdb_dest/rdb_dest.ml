
module Client = Client
module Video = Video
module Source = Source

module type DBC = Caqti_lwt.CONNECTION
module Bopt = BatOption

open Lwt.Infix
open Lib.Infix

let spf = Printf.sprintf
let plf fmt = Printf.ksprintf (print_endline) fmt

module type Config = sig
  val db_pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t
  (* [files_path] Where to save video and thumbnail files to. *)
  val files_path : string
end

let local_scheme = "local"

let make_local_uri rel_path filename =
  spf "%s:///%s/%s" local_scheme rel_path filename

let video_ext t = 
  let vid_id = Video.id t =?: 0 in
  let uri = Video.file_uri t =?: Uri.empty in
  ( Uri.path uri |> File.ext =?: (Video.filename t |> File.ext =?: "mov") )
  |> spf "%d.%s" vid_id

let thumb_ext t =
  let vid_id = Video.id t =?: 0 in
  let uri = Video.thumbnail_uri t =?: Uri.empty in
  let ext = (uri |> Uri.path |> File.ext =?: "jpeg")
            |> spf "%d.%s" vid_id in
  (* In case some joker used the same extension on both the image and video,
     we want to avoid file name collisions. *)
  if ext = (video_ext t) then "thumb." ^ ext else ext

let media_id_of_video t =
  Video.canonical t |> Source.media_id

module Make (Log : Sync.Logger) (Conf : Config) = struct
  open Conf

  type t = Video.t

  (** [get_video ~ovp ~media_id] retrieves the video attached to the [ovp]  
      (Source.name) and [media_id] combination, if any. *)
  let get_video ~ovp ~media_id = 
    Conf.db_pool |> Caqti_lwt.Pool.use begin fun (module DB : DBC) ->
      match%lwt Select.source (module DB) ~name:ovp ~media_id with
      | None -> Lwt.return (Ok None)
      | Some s -> match Source.video_id s with
        | None -> Lwt.return (Ok None)
        | Some id -> match%lwt Select.video (module DB) id with 
          | None -> Lwt.return (Ok None)
          | Some t -> Lwt.return (Ok (Some t))
    end >>= Caqti_lwt.or_fail

  let gen_file_paths t =
    let canonical = Video.canonical t in
    let rel = Source.added canonical |> File.dir_of_timestamp in
    let abs = spf "%s/%s" Conf.files_path rel in
    let basename = File.basename (Video.filename t) in
    (abs, rel, basename)

  let abs_path_of_uri uri =
    Conf.files_path ^ (Uri.path uri)

  let maybe_update_video_file_path (module DB : DBC) t new_t =
    let (_, rel_path, basename) = gen_file_paths new_t in 
    let ext = video_ext new_t in
    let filename = File.restrict_name_length basename ext in
    let new_uri = make_local_uri rel_path filename in
    let old_uri = t |> Video.file_uri |> Bopt.get |> Uri.to_string in
    if new_uri <> old_uri then 
      let media_id = media_id_of_video t in
      Log.infof "[%s] Moving video file from [%s] to [%s]"
        media_id old_uri new_uri >>= fun () ->
      let new_abs_path = abs_path_of_uri (Uri.of_string new_uri) in
      let old_abs_path
        = abs_path_of_uri (t |> Video.file_uri |> Bopt.get) in
      Lwt_unix.rename old_abs_path new_abs_path >>= fun () ->
      let vid_id = Video.id t |> Bopt.get in
      Update.video_file_uri (module DB) vid_id new_uri >|= fun () ->
      { new_t with file_uri = Some (Uri.of_string new_uri) }
    else 
      Lwt.return t

  let save_thumb_file (module DB : DBC)
    ~vid_id ~media_id ~uri ~abs_path ~rel_path ~basename ~ext
  =
    let filename = File.restrict_name_length basename ext in
    let file_path = spf "%s/%s" abs_path filename in

    begin try%lwt File.save uri ~to_:file_path >|= fun () -> Ok () with 
    | File.File_error _ as exn -> raise exn
    | exn ->
      Log.warnf ~exn "[%s] Failed saving thumbnail [%s] to [%s]"
        media_id (uri |> Uri.to_string) file_path >|= fun () ->
      Error exn
    end >>= function
    | Error _ as e ->
      File.unlink_if_exists file_path >|= fun () ->
      e
    | Ok _ ->
      let local_path = spf "%s:///%s/%s" local_scheme rel_path filename in
      Update.video_thumbnail_uri (module DB) vid_id local_path >|= fun () ->
      Ok (Uri.of_string local_path)

  let save_video_file (module DB : DBC)
    ~vid_id ~media_id ~uri ~abs_path ~rel_path ~basename ~ext
  =
    let filename = File.restrict_name_length basename ext in
    let file_path = spf "%s/%s" abs_path filename in
    
    begin try%lwt File.save uri ~to_:file_path >|= fun () -> Ok () with
    (* 9.b if file save fails, raise fatal error *)
    | File.File_error _ as exn -> raise exn
    (* 9.a if download fail, warn and continue *)
    | exn ->
      Log.warnf ~exn "[%s] Failed saving video [%s] to [%s]"
        media_id (uri |> Uri.to_string) file_path >|= fun () ->
      Error exn
    end >>= function 
    | Error _ as e ->
      File.unlink_if_exists file_path >|= fun () ->
      e
    | Ok _ ->
      (* 10. update file_uri with local URI *)
      let local_path = make_local_uri rel_path filename in
      Update.video_file_uri (module DB) vid_id local_path >>= fun () ->
      (* 11. calculate file md5 and save to DB *)
      let md5 = Digest.file file_path |> Digest.to_hex in
      Update.video_md5 (module DB) vid_id md5 >|= fun () ->
      let file_uri = Uri.of_string local_path in
      Ok (file_uri, md5)

  let move_temp_file temp_uri =
    let temp_path = abs_path_of_uri temp_uri in
    let suffix_ptrn = Re.Perl.compile_pat "\\.temp$" in
    let final_uri = temp_uri
      |> Uri.to_string 
      |> Re.replace_string ~all:false suffix_ptrn ~by:"" 
      |> Uri.of_string in
    let final_path = final_uri |> Uri.to_string in
    Lwt_unix.rename temp_path final_path >|= fun () ->
    final_uri

  let save_new (module DB : DBC) t =
    let media_id = media_id_of_video t in

    Log.infof "[%s] Inserting into DB." media_id >>= fun () ->
    Insert.video (module DB) t >>= fun t ->
    let vid_id = Video.id t |> BatOption.get in

    match Video.(file_uri t, thumbnail_uri t) with
    | None, None -> 
      Log.infof "[%s] No file or thumbnail URIs." media_id >>= fun () ->
      Lwt.return t
    | file_uri, thumb_uri ->
      let abs_path, rel_path, basename = gen_file_paths t in
      File.prepare_dir ~prefix:Conf.files_path rel_path >>= fun _ ->

      begin match thumb_uri with
      | None -> Lwt.return t
      | Some uri ->
        begin
          let ext = thumb_ext t in
          Log.infof "[%s] Saving thumbnail to [%s/%s.%s]"
            media_id rel_path basename ext >>= fun () ->
          save_thumb_file (module DB) ~vid_id ~media_id ~uri
                          ~abs_path ~rel_path ~basename ~ext 
          >|= function
          | Error _ -> t
          | Ok uri -> { t with thumbnail_uri = Some uri }
        end
      end >>= fun t ->

      begin match file_uri with 
      | None -> Lwt.return t
      | Some uri ->
        begin
          let ext = video_ext t in
          Log.infof "[%s] Saving video file to [%s/%s.%s]"
            media_id rel_path basename ext >>= fun () ->
          save_video_file (module DB) ~vid_id ~media_id ~uri
                          ~abs_path ~rel_path ~basename ~ext
          >|= function
          | Error _ -> t
          | Ok (local_uri, md5) ->
            { t with file_uri = Some local_uri; md5 = Some md5 }
        end
      end >|= fun t ->
      t

  let save_existing (module DB : DBC) t_id new_t =
    let%lwt old_t = match%lwt Select.video (module DB) t_id with
    | None -> raise Not_found
    | Some t -> Lwt.return t
    in
    
    let t = Video.{ new_t with
      id = old_t.id;
      thumbnail_uri = old_t.thumbnail_uri;
      file_uri = old_t.file_uri;
      md5 = old_t.md5;
      canonical = { new_t.canonical with id = old_t.canonical.id }
    } in

    let media_id = media_id_of_video t in

    Log.infof "[%s] Updating fields in DB." media_id >>= fun () ->
    Update.video (module DB) t >>= fun t ->

    let abs_path, rel_path, basename = gen_file_paths t in
    File.prepare_dir ~prefix:Conf.files_path rel_path >>= fun _ ->

    begin match Video.thumbnail_uri t with
    | None -> Lwt.return t
    | Some uri -> 
      begin
        let ext = spf "%s.temp" (thumb_ext t) in
        Log.infof "[%s] Saving thumbnail to [%s/%s.%s]"
          media_id rel_path basename ext >>= fun () ->
        save_thumb_file (module DB) ~vid_id:t_id ~media_id ~uri
                        ~abs_path ~rel_path ~basename ~ext 
        >>= function
        | Error _ -> Lwt.return t
        | Ok uri ->
          Log.infof "[%s] Moving thumbnail to permanent location." media_id
            >>= fun () ->
          let%lwt uri = move_temp_file uri in
          Update.video_thumbnail_uri (module DB) t_id (Uri.to_string uri)
            >|= fun () ->
          { t with thumbnail_uri = Some uri }
      end
    end >>= fun t ->

    begin match Video.file_uri t with
    | None -> Lwt.return t
    | Some uri ->
      begin
        if Video.md5 new_t = Video.md5 t && Video.file_uri t |> Bopt.is_some 
        (* 5: If file unchanged, but basename has changed, move file *)
        then maybe_update_video_file_path (module DB) t new_t
        else
          let ext = spf "%s.temp" (video_ext t) in
          Log.infof "[%s] Saving video to [%s/%s.%s]"
            media_id rel_path basename ext >>= fun () ->
          save_video_file (module DB) ~vid_id:t_id ~media_id ~uri
                          ~abs_path ~rel_path ~basename ~ext
          >>= function
          | Error _ -> Lwt.return t
          | Ok (temp_uri, md5) ->
            Log.infof "[%s] Moving video to permanent location." media_id
              >>= fun () ->
            let%lwt uri = move_temp_file temp_uri in
            Update.video_file_uri (module DB) t_id (Uri.to_string uri)
              >|= fun () ->
            { t with file_uri = Some uri; md5 = Some md5 }
      end
    end >|= fun t ->
    t
  
  let save t =
    let canonical = Video.canonical t in
    let media_id = media_id_of_video t in
    Log.infof "Saving [%s]..." media_id >>= fun () ->
    
    Conf.db_pool |> Caqti_lwt.Pool.use begin fun (module DB : DBC) ->
      Log.infof "[%s] Checking for existing video..." media_id >>= fun () ->
      let%lwt existing = Select.source (module DB)
        ~name:(Source.name canonical) ~media_id:(Source.media_id canonical) in
      
      begin match existing with
      | None
      | Some { video_id = None } ->
        Log.infof "[%s] Not saved before. Saving new video..." media_id
          >>= fun () ->
        save_new (module DB) t
      | Some { video_id = Some vid_id } ->
        Log.infof "[%s] Already exists as [%d]. Updating..."
          media_id vid_id >>= fun () ->
        save_existing (module DB) vid_id t
      end >>= fun vid ->
      Lwt.return (Ok ())
    end >>= Caqti_lwt.or_fail >>= fun () ->

    Log.infof "[%s] Finished saving." media_id >|= fun () ->
    t

end