
open Base
open Printf
open Lwt.Infix

open Lib.Infix.Function
open Lib.Infix.Option
open Lib.Lwt_result.Just_let_syntax

module Lwt_result = Lib.Lwt_result


module type Config = sig
  val params : Jw_client.Platform.param list
  val temp_pub_tag : string
  val backup_expires_field : string
end


module type Made = sig
  type t = Jw_client.Platform.videos_list_video
         * (string * int option * int option) option
         * string option

  val make_stream
    : should_sync:(t -> bool Lwt.t) -> stop_flag:(bool ref) -> t Lwt_stream.t
  val cleanup : t -> (unit, exn) Lwt_result.t
  val final_cleanup : unit -> unit Lwt.t
end


let original_thumb_url media_id = 
  sprintf "https://cdn.jwplayer.com/thumbs/%s.jpg" media_id


module Make (Client : Jw_client.Platform.Client)
            (Var_store : Sync.Variable_store)
            (Log : Logger.Sig)
            (Conf : Config)
            : Made =
struct
  let var_store = (module Var_store : Sync.Variable_store)


  type accounts_template = Jw_client.Platform.accounts_templates_list_template
  type videos_video = Jw_client.Platform.videos_list_video
  type videos_conversion = Jw_client.Platform.videos_conversions_list_conversion
  type t = videos_video
         * (string * int option * int option) option
         * string option

    
  let get_set offset =
    let params = Jw_client.Util.merge_params
      Conf.params
      [ "result_offset", [offset |> Int.to_string] ]
    in

    let%bind { videos; _ } = Client.videos_list ~params () in
    Lwt_result.return videos


  let sleep_if_few_left l =
    match List.length l with
    | 0 -> Lwt.return ()
    | count ->
      match 15 - count/3 with
      | s when s > 0 ->
        Log.infof "--> Only %d videos left processing; waiting %d seconds before checking again."
          (List.length l) s >>= fun () ->
        Lwt_unix.sleep (s |> Float.of_int) 
      | _ ->  Lwt.return ()


  let get_status_and_passthrough media_id =
    let params = [("cache_break", [Random.bits () |> Int.to_string])] in

    match%map Jw_client.Delivery.get_media media_id ~params () with
    | Some { playlist = []; _ } ->
      (true, None)

    | None ->
      (false, None)

    | Some { playlist = media :: _; _ } -> 
      let passthrough =
        media.sources
        |> List.find ~f:begin fun s -> 
          match Jw_client.V2_media_body_t.(s.label) with
          | None -> false
          | Some l -> String.(equal (lowercase l) "passthrough")
        end
      in
      (true, passthrough)


  let publish_video (vid : Jw_client.Platform.videos_list_video) =
    let tags = vid.tags
      |> String.split_on_chars ~on:[',']
      |> List.map ~f:(String.strip ?drop:None)
      |> (fun l -> Conf.temp_pub_tag :: l)
      |> String.concat ~sep:", "
    in

    let backup_expires_field = "custom." ^ Conf.backup_expires_field in
    let expires_date =
      Option.value_map ~default:"" ~f:Int.to_string vid.expires_date in

    let params =
      [ ("expires_date", [""])
      ; (backup_expires_field, [expires_date])
      ; ("tags", [tags]) ]
    in

    Client.videos_update vid.key params


  let refresh_current_videos_set ~returned =
    let%lwt offset = Var_store.get "request_offset" ~default:"0" () in
    let%bind vids = get_set (offset |> Int.of_string) in

    let to_check =
      vids |> List.filter ~f:begin fun (v : videos_video) ->
        returned
        |> List.find ~f:(fun (r : videos_video) -> String.equal r.key v.key)
        |> Option.is_none
      end
    in

    Lwt_result.return (vids, to_check)


  (* Cache the passthrough template key. This will save nearly 1 API request
     per video processed. *)
  let passthrough_template_key = ref None

  let add_passthrough_conversion media_id =
    let%bind_open key =
      match !passthrough_template_key with
      | Some k -> return k

      | None ->
        let%bind body = Client.accounts_templates_list () in

        body.templates

        |> List.find ~f:begin fun (t : accounts_template) ->
          String.(equal (lowercase t.name) "passthrough")
        end

        |> function
          | None -> 
            fail @@ Not_found_s
              (Parsexp.Single.parse_string_exn
                "passthrough template not found.")

          | Some { key; _ } ->
            let () = passthrough_template_key := Some key in
            return key
    in

    Client.videos_conversions_create media_id key


  let prepare_video_for_sync (vid : videos_video) ~published ~passthrough =
    let%lwt prev_changes = Changes.get_record var_store vid.key in
    let now = Unix.time () |> Int.of_float in

    let%bind_open changes = 
      match published, vid.expires_date with
      | true, _ -> return prev_changes

      | false, None ->
        let%lwt () = Log.infof "[%s] Waiting on publish." vid.key in
        return prev_changes

      | false, Some e when e > now ->
        let%lwt () = Log.infof "[%s] Waiting on publish." vid.key in
        return prev_changes

      | false, Some _ ->
        let%lwt () = Log.infof "[%s] Not published; publishing..." vid.key in

        let changes = { prev_changes with expires = vid.expires_date } in
        let%lwt () = Changes.set_record var_store vid.key changes in
        let%bind () = publish_video vid in

        return changes
    in

    begin match passthrough with
    | Some _ -> Lwt_result.return ()

    | None ->
      let%bind_open needs_passthrough = 
        if not changes.passthrough then return true
        else

        let%bind { conversions; _ } = Client.videos_conversions_list vid.key in

        let passthrough =
          conversions
          |> List.find ~f:begin fun (c : videos_conversion) ->
            String.(equal (lowercase c.template.name) "passthrough")
          end
        in

        match passthrough with
        | Some { status = `Failed; key; _ } ->
          (* @todo Deal with passthrough conversions that fail every time.
                  This has the potential to infinitely loop. *)
          let%lwt () =
            Log.errorf "[%s] Passthrough conversion creation failed."  vid.key
          in
          let%bind () = Client.videos_conversions_delete key in
          return true

        | Some _ | None ->
          return false
      in

      if needs_passthrough then
        let%lwt () = Log.infof "[%s] No passthrough; creating..." vid.key in
        let changes' = { changes with passthrough = true } in
        let%lwt () = Changes.set_record var_store vid.key changes' in

        add_passthrough_conversion vid.key
      else
        Lwt_result.return_lwt @@
          Log.infof "[%s] Waiting on passthrough." vid.key
    end


  let cleanup_by_media_id media_id ?changed () =
    let%lwt () = Log.debugf "Undoing changes to [%s]." media_id in

    let%lwt { expires; passthrough; _ } =
      match changed with 
      | None -> Changes.get_record var_store media_id
      | Some c -> Lwt.return c 
    in

    let%bind_open () =
      match expires with 
      | None ->
        return ()

      | Some expires_date ->
        let%lwt () = Log.debugf "[%s] Undoing publish." media_id in

        match%bind Client.videos_show media_id with
        | None ->
          return ()

        | Some { video; _ } ->
          (* Make sure we have the latest expires date in case it was set
            outside of this program. *)
          let expires_date' = video.expires_date =?: expires_date in

          let tags = video.tags
            |> String.split_on_chars ~on:[',']
            |> List.map ~f:(String.strip ?drop:None)
            |> List.filter ~f:(String.equal Conf.temp_pub_tag %> not)
            |> String.concat ~sep:", "
          in

          (* "-" prefix tells JW to remove the custom field *)
          let backup_expires_field = "custom.-" ^ Conf.backup_expires_field in

          let params =
            [ ("expires_date", [expires_date' |> Int.to_string])
            ; (backup_expires_field, [""])
            ; ("tags", [tags]) ]
          in

          Client.videos_update media_id params
    in
    
    let%bind_open () = 
      if not passthrough then return ()
      else

      let%lwt () = Log.debugf "[%s] Deleting passthrough conversion" media_id in

      match%lwt Client.delete_conversion_by_name media_id "passthrough" with
      | Error (Not_found_s _) (* Likely deleted outside this program. *)
      | Ok () ->
        let%lwt () = Changes.clear_record var_store media_id in
        return ()

      | Error e ->
        fail e
    in

    let%lwt () =
      if passthrough || Option.is_some expires
      then Log.infof "[%s] Undid all changes." media_id
      else Log.debugf "[%s] No changes to undo." media_id
    in

    Lwt_result.return ()


  let cleanup ((vid, _, _) : t) = cleanup_by_media_id vid.key ()


  let cleanup_old_changes ~exclude ?(min_age=0) () = 
    Changes.get_all_records var_store ~except:exclude ~min_age ()

    >>= Lwt_list.iter_p begin fun (media_id, changed) ->
      match%lwt cleanup_by_media_id media_id ~changed () with
      | Error exn ->
        let%lwt () = Log.errorf ~exn "[%s] Failed cleaning up." media_id in
        Lwt.return ()

      | Ok () ->
        Lwt.return ()
    end


  let final_cleanup () = cleanup_old_changes ~exclude:[] ()


  (** [clear_temp_changes_for_return ?changed vid] Makes sure any changes to
      [vid] that are just to facilitate the sync process are not actually
      synced to the destination. *)
  let clear_temp_changes_for_return ?changed (vid : videos_video) =
    let%lwt { expires; _ } =
      match changed with 
      | None -> Changes.get_record var_store vid.key
      | Some c -> Lwt.return c 
    in
    
    if Option.is_none expires then Lwt.return vid
    else

    let tags =
      vid.tags
      |> String.split_on_chars ~on:[',']
      |> List.filter ~f:(String.equal Conf.temp_pub_tag %> not)
      |> String.concat ~sep:", " in   

    let custom = 
      vid.custom
      |> List.filter ~f:((fst %> String.equal Conf.backup_expires_field)) 
    in

    Lwt.return { vid with expires_date=expires; tags; custom }


  let log_request_failures = function
    | [] ->
      Lwt.return ()

    | lst -> 
      lst
      |> List.map
        ~f:(fun (meth, url, err) -> sprintf "--> %s [%s %s]" err meth url)
      |> String.concat ~sep:"\n"
      |> Log.warnf "Enountered temporary request errors:\n%s"


  (* [make_stream should_sync] streams [t] until it has run through all of them. 
   * Note that it remembers the last offset and continues from there assuming
   * the [Variable_store] passed to [Make] is persistent.
   * 
   * JW does three things that complicate this process:
   *
   *   1) Original media files are unavailable by default, requiring a 
   *      "passthrough" conversion to be added, which is not an instant process.
   *      @see http://qa.jwplayer.com/~abussey/demos/general/access-originals.html
   * 
   *   2) The passthrough conversion counts against space usage.
   *
   *   3) Conversions of unpublished ("expired") media are inaccessible. 
   *
   * This means we need to prepare most media before returning it and then undo
   * any changes that we made. 
   *)
  let make_stream
      ~(should_sync : t -> bool Lwt.t) 
      ~stop_flag
  : t Lwt_stream.t =
    let current_videos_set = ref [] in
    let videos_to_check = ref [] in
    let processing_videos = ref [] in
    (* Keep track of requests that fail for likely temporary reasons to be
       reported just before finishing sync. Since failed requests lead to
       skipping to the next item, we want to log these in case the user
       wants to follow up on the items that were not synced. *)
    let failed_requests = ref [] in

    let rec next () =
      if !stop_flag then
        let%lwt () = Log.info "Stop flag set. Stopping sync." in
        let%lwt () = log_request_failures !failed_requests in
        Lwt_result.return None
      else

      let%bind_open () = 
        match !videos_to_check, !processing_videos with
        | [], [] ->
          (* Check for new videos at the current offset in case more were 
            added (or some removed, pushing more into the current offset) in 
            the time it took to process the current set *)
          let%bind_open refreshed =
            match !current_videos_set with
            (* First set, no need to refresh *)
            | [] -> return ([], []) 
            (* Since [videos_to_check] and [processing_videos] are empty, we can
              assume that all videos in [current_videos_set] have been returned.
              *)
            | returned -> refresh_current_videos_set ~returned
          in

          begin match refreshed with
          | _, [] ->
            let%lwt () = Log.debug "Getting next set." in
            let%lwt offset = Var_store.get "request_offset" ~default:"0" () in

            let new_offset =
              (offset |> Int.of_string) + (List.length !current_videos_set) in
            let%lwt () =
              Var_store.set "request_offset" (new_offset |> Int.to_string) in
            let%lwt () = Log.debugf "--> offset: %d" new_offset in

            let%bind vids = get_set new_offset in

            let () = current_videos_set := vids in
            let () = videos_to_check := vids in

            let%lwt () = Log.infof "Got new set of %d videos at offset %d"
              (List.length vids) new_offset in

            let%lwt () = Log.debug "Cleaning up old changes." in
            let exclude = 
              !videos_to_check
              |> List.map ~f:(fun (v : videos_video) -> v.key)
            in

            return_lwt @@
              cleanup_old_changes ~exclude ~min_age:(12 * 60 * 60) ()

          | refreshed_set, refreshed_to_check ->
            let%lwt () = Log.infof
              "Returned all vidoes in current set, but %d more were added at the current offset during that time. Processing those." 
              (List.length refreshed_to_check)
            in
            let () = current_videos_set := refreshed_set in
            let () = videos_to_check := refreshed_to_check in
            return ()
          end

          | [], _ -> 
            let%lwt () = Log.debug
              "List of videos to check exhausted. Refreshing data of those still in processing and setting them up to be checked again."
            in

            (* Be sure we grab any updates that happened outside this program 
              * during the last pass. *)
            let returned =
              !current_videos_set
              |> List.filter ~f:begin fun (c : videos_video) ->
                !processing_videos 
                |> List.find
                  ~f:(fun (p : videos_video) -> String.equal p.key c.key)
                |> Option.is_none
              end
            in

            let%bind (refreshed_set, refreshed_to_check) =
              refresh_current_videos_set ~returned
            in

            let%lwt () = Log.infof
              "Checking on %d videos marked as processing."
              (List.length refreshed_to_check)
            in

            let () = 
              current_videos_set := refreshed_set;
              videos_to_check := refreshed_to_check;
              processing_videos := [];
            in

            return_lwt @@ sleep_if_few_left !videos_to_check 

          | _, _
            -> return ()
      in

      match !videos_to_check with
      | [] -> 
        let%lwt () = log_request_failures !failed_requests in
        let%lwt () = Log.info "Processed all videos at source." in
        let%lwt () = Var_store.delete "request_offset" in
        Lwt_result.return None

      | vid :: tl ->
        let%lwt () = Log.debugf "Checking [%s: %s]" vid.key vid.title in
        let () = videos_to_check := tl in

        let%lwt sync_needed =
          let%lwt v = clear_temp_changes_for_return vid in
          should_sync (v, None, None)
        in

        match sync_needed, vid.status, vid.sourcetype with
        | false, _, _ ->
          let%lwt () = Log.infof "[%s] No need to sync. NEXT!" vid.key in
          (* Just in case it was found not to need to be synced after changes
             were made to it in prep for sync. *)
          let%bind () = cleanup_by_media_id vid.key () in
          next ()

        | true, (`Created | `Processing | `Updating | `Failed), `File
        | true, _, `URL ->
          let%lwt () = Log.infof
            "[%s] has URL source or non-ready status. RETURNING!" vid.key in

          (* Since this program is designed to be run over and over again, 
             constantly syncing media from JW, we can catch anything that's
             processing the next time we reach this offset. *)
          let%lwt vid = clear_temp_changes_for_return vid in
          let file = vid.sourceurl >|? (fun s -> (s, None, None)) in
          let thumb = original_thumb_url vid.key in
          Lwt_result.return (Some (vid, file, Some thumb))

        | true, `Ready, `File ->
          let%lwt () = Log.debugf
            "[%s] Getting publish and passthrough status." vid.key in

          match%bind get_status_and_passthrough vid.key with
          | true, Some { file; width; height; _ } ->
            let%lwt () = Log.infof
              "[%s] Video is published and has passthrough. RETURNING!"
              vid.key
            in

            let%lwt vid = clear_temp_changes_for_return vid in
            let thumb = original_thumb_url vid.key in

            Lwt_result.return @@
              Some (vid, Some (file, width, height), Some thumb)

          | published, passthrough ->
            match%lwt prepare_video_for_sync vid ~published ~passthrough with
            | Error (Not_found_s _) ->
              let%lwt () =
                Log.infof "[%s] Looks like this video no longer exists. NEXT!"
                vid.key
              in
              next ()
            
            | Error e ->
              Lwt_result.fail e

            | Ok () ->
              let%lwt () = Log.debugf
                "[%s] Adding to processing list. NEXT!" vid.key in
              let () = processing_videos := vid :: !processing_videos in

              (* This video isn't ready to be returned yet, take at the next
                 video in the list. This video will get looked at again once
                 we've gone through every video in the current list to check. *)
              next ()

    in

    let rec try_next () =
      match%lwt next () with
      | Error (Jw_client.Exn.Timeout (meth, uri)) ->
        let () = stop_flag := true in
        let%lwt () = Log.warnf "Request timed out: [%s %s]" meth uri in

        (* @todo Add method to recover from timeout errors without completely
                 skipping the item. Probably need to wrap
                 [match !videos_to_check with ...] section with [try] *)
        let () =
          failed_requests := (meth, uri, "timed out") :: !failed_requests in

        try_next ()

      | Error
          (Jw_client.Exn.Unexpected_response_status (method_, path, response))
          ->
        let () = stop_flag := true in
        let%lwt () = Log.fatalf
          "Unexpected HTTP response\n\
            --> Request: [%s %s]\n\n\
            --> Response <--\n%s\n\n"
          method_ path response
        in
        let%lwt () = log_request_failures !failed_requests in
        Lwt.return None

      | Error (Jw_client.Exn.Temporary_error (meth, uri, exn)) ->
        let%lwt () = Log.warnf
          "Temporary error making request [%s %s]: %s"
          meth uri exn
        in
        let () = failed_requests := (meth, uri, exn) :: !failed_requests in
        try_next ()

      | Error exn ->
        let () = stop_flag := true in
        let%lwt () = Log.fatalf ~exn "Unexpected error" in
        let%lwt () = log_request_failures !failed_requests in
        Lwt.return None

      | Ok video ->
        Lwt.return video
    in

    Lwt_stream.from try_next
end