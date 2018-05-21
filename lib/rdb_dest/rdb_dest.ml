
module Client = Client
module Video = Video
module Source = Source

module type DBC = Caqti_lwt.CONNECTION

open Lwt.Infix
open Lib.Infix

let spf = Printf.sprintf

module type Config = sig
  val db_pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t
  (* [files_path] Where to save video and thumbnail files to. *)
  val files_path : string
end

let local_scheme = "local"

let media_id_of_video t =
  Video.canonical t |> Source.media_id

module Make (Log : Sync.Logger) (Conf : Config) = struct
  open Conf

  type t = Video.t

  let save_new (module DB : DBC) t =
    (* 1. Save new video as is *)
    Insert.video (module DB) t >>= fun t ->
    let vid_id = Video.id t |> BatOption.get in
    let media_id = media_id_of_video t in
    (* 2. return if thumbnail_uri and file_uri are None *)
    match Video.(file_uri t, thumbnail_uri t) with
    | None, None -> 
      Lwt.return t
    | file_uri, thumb_uri ->
      let canonical = Video.canonical t in
      (* 3. figure out directory based on canonical.added *)
      let rel_path = Source.added canonical |> File.dir_of_timestamp in
      (* 4. prepare directory *)
      let%lwt abs_path = File.prepare_dir ~prefix:Conf.files_path rel_path in
      (* 5. figure out basename based on filename, slug, then title
            consider max filename length *)
      let basename = File.basename (Video.filename t) in

      begin match thumb_uri with
      | None -> Lwt.return (t, "")
      | Some uri ->
        (* 6. figure out thumbnail extension based on URI, defaulting to .jpeg *)
        let ext = uri |> Uri.path |> File.ext =?: "jpeg"
                  |> spf "%d.%s" vid_id in
        let filename = File.restrict_name_length basename ext in
        let file_path = spf "%s/%s" abs_path filename in
        (* 7. save thumbnail via streaming *)
        begin try%lwt
          File.save uri ~to_:file_path >>= fun () ->
          (* 8. update thumbnail_uri with local URI *)
          let local_path = spf "%s:///%s/%s" local_scheme rel_path filename in
          Update.video_thumbnail_uri (module DB) vid_id local_path
            >|= fun () ->
          let thumbnail_uri = Some (Uri.of_string local_path) in
          { t with thumbnail_uri }
        with 
        (* 7.b if file save fails, raise fatal error *)
        | File.File_error _ as exn -> raise exn
        (* 7.a if download fail, warn and continue *)
        | exn ->
          Log.warnf ~exn "[%s] Failed saving thumbnail [%s] to [%s]"
            media_id (uri |> Uri.to_string) file_path >|= fun () ->
          t
        end >|= fun t -> (t, ext)
      end >>= fun (t, thumb_ext) ->

      begin match file_uri with 
      | None -> Lwt.return t
      | Some uri ->
        let ext = Uri.path uri |> File.ext
                =?: Video.filename t |> File.ext =?: "mov"
                |> spf "%d.%s" vid_id in
        let ext = if ext = thumb_ext then "video." ^ ext else ext in
        let filename = File.restrict_name_length basename ext in
        let file_path = spf "%s/%s" abs_path filename in
        
        begin try%lwt
          File.save uri ~to_:file_path >>= fun () ->
          (* 10. update file_uri with local URI *)
          let local_path = spf "%s:///%s/%s" local_scheme rel_path filename in
          Update.video_file_uri (module DB) vid_id local_path >>= fun () ->
          (* 11. calculate file md5 and save to DB *)
          let md5 = Digest.file file_path |> Digest.to_hex in
          Update.video_md5 (module DB) vid_id md5 >|= fun () ->
          let file_uri = Some (Uri.of_string local_path) in
          { t with file_uri; md5 = Some md5 }
        with
        (* 9.b if file save fails, raise fatal error *)
        | File.File_error _ as exn -> raise exn
        (* 9.a if download fail, warn and continue *)
        | exn ->
          Log.warnf ~exn "[%s] Failed saving video [%s] to [%s]"
            media_id (uri |> Uri.to_string) file_path >|= fun () ->
          t
        end
      end >>= fun t ->
      (* 12. return updated t *)
      Lwt.return t

  let save_existing (module DB : DBC) t_id new_t =
    let%lwt old_t = match%lwt Select.video (module DB) t_id with
    | None -> raise Not_found
    | Some t -> Lwt.return t
    in
    Lwt.return old_t
  
  let save t =
    let canonical = Video.canonical t in
    let media_id = media_id_of_video t in
    Log.infof "Saving [%s]..." media_id >>= fun () ->
    
    Conf.db_pool |> Caqti_lwt.Pool.use begin fun (module DB : DBC) ->
      (* 1: Check if a video with one of the same ovp+media exists. *)
      let%lwt existing = Select.source (module DB)
        ~name:(Source.name canonical) ~media_id:(Source.media_id canonical) in
      
      begin match existing with
      (* 2b: If not, save new video *)
      | None
      | Some { video_id = None } ->
        save_new (module DB) t
      | Some { video_id = Some vid_id } ->
        save_existing (module DB) vid_id t
      end >>= fun vid ->
      (* 3: Update existing sources *)
      (* 4: Save new sources *)
      (* 5: Update canonical source if changed *)
      (* 6: Save custom data *)
      (* 7: Save tags *)
      (* 8: [jwsrc] Try getting feed *)
      (* 9a: [jwsrc] If exists, set file and thumb URIs *)
      (* 9b: [jwsrc] If non-existent, guess at file and thumb URIs? or set None *)
      (* 10: Use MD5 to see if existing file needs updating
      *     Be sure JW updates the MD5 when the original video file is
      *     replaced. *)
      (* 11: Prepare directory to save files *)
      (* 12: Save video file if new/changed *)
      (* 14: Delete old video file if it wasn't replaced *)
      (* 15: Save thumbnail file if new/changed *)
      (* 16: Delete old thumbnail file if it wasn't replaced *)
      (* 17: [jwsrc] Clean up passthrough conversion & expiration *)
      Lwt.return (Ok ())
    end >>= Caqti_lwt.or_fail >>= fun () ->

    Lwt.return t

end