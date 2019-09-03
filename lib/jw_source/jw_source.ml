
open Lwt.Infix
open Lib.Infix
open Printf

module Bopt = BatOption


module Modified = struct
  open Sexplib.Std

  type t = {
    timestamp : int;
    expires : int option;
    passthrough : bool;
  } [@@deriving sexp]

  let make ?expires ~passthrough () =
    { timestamp = Unix.time () |> int_of_float;
      expires; passthrough }

  let to_string t =
    t |> sexp_of_t |> Sexplib.Sexp.to_string

  let of_string s =
    s |> Sexplib.Sexp.of_string |> t_of_sexp
end


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
  val cleanup : t -> unit Lwt.t
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
  type accounts_template = Jw_client.Platform.accounts_templates_list_template
  type videos_video = Jw_client.Platform.videos_list_video
  type videos_conversion = Jw_client.Platform.videos_conversions_list_conversion
  type t = videos_video
         * (string * int option * int option) option
         * string option


  let changed_video_key media_id = "video-changed-" ^ media_id

  let get_changed media_id =
    let key = changed_video_key media_id in
    Var_store.get_opt key >>= function
    | None   -> Lwt.return @@ Modified.make ~passthrough:false ()
    | Some v -> Lwt.return @@ Modified.of_string v

  let set_changed media_id changes =
    let key = changed_video_key media_id in
    let value = changes |> Modified.to_string in
    Var_store.set key value

  let clear_changed media_id =
    let key = changed_video_key media_id in
    Var_store.delete key

    
  let get_set offset =
    let params = Jw_client.Util.merge_params
      Conf.params
      [ "result_offset", [offset |> string_of_int] ]
    in
    let%lwt { videos; _ } = Client.videos_list ~params () in
    Lwt.return videos


  let sleep_if_few_left l =
    match List.length l with
    | 0 -> Lwt.return ()
    | count ->
      match 15 - count/3 with
      | s when s > 0 ->
        Log.infof "--> Only %d videos left processing; waiting %d seconds before checking again."
          (List.length l) s >>= fun () ->
        Lwt_unix.sleep (s |> float_of_int) 
      | _ ->  Lwt.return ()


  let get_status_and_passthrough media_id =
    let params = [("cache_break", [Random.bits () |> string_of_int])] in

    match%lwt Jw_client.Delivery.get_media media_id ~params () with
    | Some { playlist = media :: _; _ } -> 
      let passthrough = media.sources |> List.find_opt begin fun s -> 
        let open Jw_client.V2_media_body_t in
        match s.label with
        | None -> false
        | Some l -> String.lowercase_ascii l = "passthrough"
      end in
      Lwt.return (true, passthrough)

    | Some { playlist = []; _ } -> Lwt.return (true, None)
    | None -> Lwt.return (false, None)


  let publish_video (vid : Jw_client.Platform.videos_list_video) =
    let tags = vid.tags
      |> String.split_on_char ','
      |> List.map String.trim
      |> (fun l -> Conf.temp_pub_tag :: l)
      |> String.concat ", "
    in
    let backup_expires_field = "custom." ^ Conf.backup_expires_field in
    let expires_date = BatOption.map_default string_of_int "" vid.expires_date
    in
    let params =
      [ ("expires_date", [""])
      ; (backup_expires_field, [expires_date])
      ; ("tags", [tags]) ]
    in
    Client.videos_update vid.key params



  let refresh_current_videos_set ~returned =
    let%lwt offset = Var_store.get "request_offset" ~default:"0" () in
    let%lwt vids = get_set (offset |> int_of_string) in
    let to_check = vids 
      |> List.filter (fun (v : videos_video) ->
        BatOption.is_none @@ List.find_opt
          (fun (r : videos_video) -> r.key = v.key) returned)
    in

    Lwt.return (vids, to_check)


  (* Cache the passthrough template key. This will save nearly 1 API request
     per video processed. *)
  let passthrough_template_key = ref None

  let add_passthroug_conversion media_id =
    begin match !passthrough_template_key with
    | Some k -> Lwt.return k
    | None ->
      let%lwt body = Client.accounts_templates_list () in
      let template = body.templates |> List.find (fun (t : accounts_template) ->
        String.lowercase_ascii t.name = "passthrough"
      ) in
      passthrough_template_key := Some template.key;
      Lwt.return template.key
    end >>= fun key ->

    Client.videos_conversions_create media_id key

  let prepare_video_for_sync (vid : videos_video) ~published ~passthrough =
    let%lwt prev_changes = get_changed vid.key in
    let now = Unix.time () |> int_of_float in

    let%lwt changes = 
      match published, vid.expires_date with
      | true, _ -> Lwt.return prev_changes

      | false, None ->
        let%lwt () = Log.infof "[%s] Waiting on publish." vid.key in
        Lwt.return prev_changes

      | false, Some e when e > now ->
        let%lwt () = Log.infof "[%s] Waiting on publish." vid.key in
        Lwt.return prev_changes

      | false, Some _ ->
        let%lwt () = Log.infof "[%s] Not published; publishing..." vid.key in

        let changes = { prev_changes with expires = vid.expires_date } in
        let%lwt () = set_changed vid.key changes in
        let%lwt () = publish_video vid in

        Lwt.return changes
    in

    begin match passthrough with
    | Some _ -> Lwt.return ()
    | None ->
      begin if changes.passthrough then
        let%lwt { conversions; _ }
          = Client.videos_conversions_list vid.key
        in
        let passthrough = conversions
          |> List.find_opt (fun (c : videos_conversion) ->
            String.lowercase_ascii c.template.name = "passthrough")
        in
        match passthrough with
        | None -> Lwt.return true
        | Some { status = `Failed; key; _ } ->
          (* @todo Deal with passthrough conversions that fail every time.
                   This has the potential to infinitely loop. *)
          Log.errorf "[%s] Passthrough conversion creation failed."  
            vid.key >>= fun () ->
          Client.videos_conversions_delete key >>= fun () ->
          Lwt.return true
        | _ ->
          Lwt.return false
      else
        Lwt.return true
      end >>= fun needs_passthrough ->

      if needs_passthrough then
        Log.infof "[%s] No passthrough; creating..." vid.key
          >>= fun () ->
        let changes' = { changes with passthrough = true } in
        set_changed vid.key changes' >>= fun () ->
        add_passthroug_conversion vid.key
      else
        Log.infof "[%s] Waiting on passthrough." vid.key
    end


  let cleanup_by_media_id media_id ?changed () =
    Log.debugf "Undoing changes to [%s]" media_id >>= fun () ->
    let%lwt { expires; passthrough; _ } = match changed with 
    | None -> get_changed media_id
    | Some c -> Lwt.return c 
    in

    begin match expires with 
    | None -> Lwt.return ()
    | Some expires_date ->
      Log.debugf "[%s] Undoing publish" media_id >>= fun () ->
      match%lwt Client.videos_show media_id with
      | None -> Lwt.return ()
      | Some { video; _ } ->
        (* Make sure we have the latest expires date in case it was set
           outside of this program. *)
        let expires_date' = match video.expires_date with
        | Some e -> e
        | None -> expires_date
        in
        let tags = video.tags
          |> String.split_on_char ','
          |> List.map String.trim
          |> List.filter (fun t -> not (t = Conf.temp_pub_tag))
          |> String.concat ", "
        in
        (* "-" prefix tells JW to remove the custom field *)
        let backup_expires_field = "custom.-" ^ Conf.backup_expires_field in
        let params =
          [ ("expires_date", [expires_date' |> string_of_int])
          ; (backup_expires_field, [""])
          ; ("tags", [tags]) ]
        in
        Client.videos_update media_id params
    end >>= fun () ->
    
    begin if passthrough then
      Log.debugf "[%s] Deleting passthrough conversion" media_id >>= fun () ->
      try%lwt Client.delete_conversion_by_name media_id "passthrough" with
      (* Likely deleted outside this program. *)
      | Not_found -> Lwt.return () 
    else
      Lwt.return ()
    end >>= fun () ->

    clear_changed media_id >>= fun () ->
    if passthrough || Bopt.is_some expires
    then Log.infof "[%s] Undid all changes." media_id
    else Log.debugf "[%s] No changes to undo." media_id


  let cleanup ((vid, _, _) : t) = cleanup_by_media_id vid.key ()


  let cleanup_old_changes ~exclude ?(min_age=0) () = 
    let now = Unix.time () |> int_of_float in
    let prefix = changed_video_key "" in
    let pattern = changed_video_key "%" in
    let%lwt changed_list = Var_store.get_like pattern in

    changed_list
    |> List.map (fun (key, changes) ->
      let (_, media_id) = BatString.replace ~str:key ~sub:prefix ~by:"" in
      (media_id, changes))
    |> List.filter (fun (media_id, _) ->
      Bopt.is_none @@ List.find_opt ((=) media_id) exclude)
    |> List.map (fun (media_id, changes) ->
      (media_id, Modified.of_string changes))
    |> List.filter (fun ((_, { timestamp; _ }) : string * Modified.t) ->
        (now - timestamp) > min_age)
    |> Lwt_list.iter_p (fun (media_id, changed) ->
      try%lwt cleanup_by_media_id media_id ~changed () with
      | exn -> Log.errorf ~exn "[%s] Failed cleaning up." media_id)

  let final_cleanup () = cleanup_old_changes ~exclude:[] ()

  (** [clear_temp_changes_for_return ?changed vid] Makes sure any changes to
      [vid] that are just to facilitate the sync process are not actually
      synced to the destination.  *)
  let clear_temp_changes_for_return ?changed (vid : videos_video) =
    let%lwt { expires; _ } =
      match changed with 
      | None -> get_changed vid.key
      | Some c -> Lwt.return c 
    in
    
    if Bopt.is_none expires then Lwt.return vid
    else

    let tags =
      String.split_on_char ',' vid.tags
      |> List.filter ((<>) Conf.temp_pub_tag)
      |> String.concat ", " in   
    let custom = 
      List.filter ((fst %> (<>) Conf.backup_expires_field)) vid.custom in

    Lwt.return { vid with expires_date=expires; tags; custom }

  let log_request_failures = function
    | [] -> Lwt.return ()
    | lst -> 
      lst
      |> List.map
        (fun (meth, url, err) -> sprintf "--> %s [%s %s]" err meth url)
      |> String.concat "\n"
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
  let make_stream ~(should_sync : (t -> bool Lwt.t)) ~stop_flag : t Lwt_stream.t =
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
        Log.info "Stop flag set. Stopping sync." >>= fun () ->
        log_request_failures !failed_requests >>= fun () ->
        Lwt.return None
      else

      begin match !videos_to_check, !processing_videos with
      | [], [] ->
        (* Check for new videos at the current offset in case more were 
           added (or some removed, pushing more into the current offset) in 
           the time it took to process the current set *)
        let%lwt refreshed = match !current_videos_set with
        (* First set, no need to refresh *)
        | [] -> Lwt.return ([], []) 
        (* Since [videos_to_check] and [processing_videos] are empty, we can
           assume that all videos in [current_videos_set] have been returned. *)
        | returned -> refresh_current_videos_set ~returned
        in

        begin match refreshed with
        | _, [] ->
          Log.debug "Getting next set." >>= fun () ->
          let%lwt offset = Var_store.get "request_offset" ~default:"0" () in
          let new_offset =
            (offset |> int_of_string) + (List.length !current_videos_set) in
          Var_store.set "request_offset" (new_offset |> string_of_int)
            >>= fun () ->
          Log.debugf "--> offset: %d" new_offset >>= fun () ->

          let%lwt vids = get_set new_offset in

          current_videos_set := vids;
          videos_to_check := vids;
          Log.infof "Got new set of %d videos at offset %d"
            (List.length vids) new_offset >>= fun () ->

          Log.debug "Cleaning up old changes." >>= fun () ->
          let exclude = !videos_to_check
            |> List.map (fun (v : videos_video) -> v.key)
          in
          cleanup_old_changes ~exclude ~min_age:(12 * 60 * 60) ()

        | refreshed_set, refreshed_to_check ->
          Log.infof "Returned all vidoes in current set, but %d more were added at the current offset during that time. Processing those." 
            (List.length refreshed_to_check) >>= fun () ->
          current_videos_set := refreshed_set;
          videos_to_check := refreshed_to_check;
          Lwt.return ()
        end

      | [], _ -> 
        Log.debug "List of videos to check exhausted. Refreshing data of those still in processing and setting them up to be checked again."
          >>= fun () ->
        (* Be sure we grab any updates that happened outside this program 
          * during the last pass. *)
        let returned = !current_videos_set
          |> List.filter (fun (c : videos_video) ->
            BatOption.is_none @@ List.find_opt
              (fun (p : videos_video) -> p.key = c.key) !processing_videos)
        in
        let%lwt (refreshed_set, refreshed_to_check) =
          refresh_current_videos_set ~returned
        in

        Log.infof "Checking on %d videos marked as processing."
          (List.length refreshed_to_check) >>= fun () ->

        current_videos_set := refreshed_set;
        videos_to_check := refreshed_to_check;
        processing_videos := [];

        sleep_if_few_left !videos_to_check 

      | _, _
        -> Lwt.return ()
      end >>= fun () ->

      match !videos_to_check with
      | [] -> 
        log_request_failures !failed_requests >>= fun () ->
        Log.info "Processed all videos at source." >>= fun () ->
        Var_store.delete "request_offset" >>= fun () ->
        Lwt.return None

      | vid :: tl ->
        Log.debugf "Checking [%s: %s]" vid.key vid.title >>= fun () ->
        videos_to_check := tl;

        let%lwt sync_needed =
          let%lwt v = clear_temp_changes_for_return vid in
          should_sync (v, None, None)
        in

        match sync_needed, vid.status, vid.sourcetype with
        | false, _, _ ->
          Log.infof "[%s] No need to sync. NEXT!" vid.key >>= fun () ->
          (* Just in case it was found not to need to be synced after changes
             were made to it in prep for sync. *)
          cleanup_by_media_id vid.key () >>= fun () ->
          next ()

        | true, (`Created | `Processing | `Updating | `Failed), `File
        | true, _, `URL ->
          Log.infof "[%s] has URL source or non-ready status. RETURNING!"
            vid.key >>= fun () ->
          (* Since this program is designed to be run over and over again, 
             constantly syncing media from JW, we can catch anything that's
             processing the next time we reach this offset. *)
          let%lwt vid = clear_temp_changes_for_return vid in
          let file = vid.sourceurl >|? (fun s -> (s, None, None)) in
          let thumb = original_thumb_url vid.key in
          Lwt.return (Some (vid, file, Some thumb))

        | true, `Ready, `File ->
          Log.debugf "[%s] Getting publish and passthrough status." vid.key
            >>= fun () ->

          match%lwt get_status_and_passthrough vid.key with
          | true, Some { file; width; height; _ } ->
            Log.infof "[%s] Video is published and has passthrough. RETURNING!"
              vid.key >>= fun () ->
            let%lwt vid = clear_temp_changes_for_return vid in
            let thumb = original_thumb_url vid.key in
            Lwt.return (Some (vid, Some (file, width, height), Some thumb))

          | published, passthrough ->
            match%lwt prepare_video_for_sync vid ~published ~passthrough with
            | exception Not_found -> 
              Log.infof "[%s] Looks like this video no longer exists. NEXT!"
                vid.key >>= fun () ->
              next ()

            | () ->
              Log.debugf "[%s] Adding to processing list. NEXT!" vid.key
                >>= fun () ->
              processing_videos := vid :: !processing_videos;
              (* This video isn't ready to be returned yet, take at the next
                 video in the list. This video will get looked at again once
                 we've gone through every video in the current list to check. *)
              next ()

    in

    let rec try_next () =
      try%lwt next () with
      | Jw_client.Exn.Timeout (meth, uri) ->
        stop_flag := true;
        Log.warnf "Request timed out: [%s %s]" meth uri >>= fun () ->
        (* @todo Add method to recover from timeout errors without completely
                skipping the item. Probably need to wrap
                [match !videos_to_check with ...] section with [try] *)
        failed_requests := (meth, uri, "timed out") :: !failed_requests;
        try_next ()

      | Jw_client.Exn.Unexpected_response_status (method', path, response) ->
        stop_flag := true;
        Log.fatalf
          "Unexpected HTTP response\n\
            --> Request: [%s %s]\n\n\
            --> Response <--\n%s\n\n"
          method' path response >>= fun () ->
        log_request_failures !failed_requests >>= fun () ->
        Lwt.return None

      | Jw_client.Exn.Temporary_error (meth, uri, exn) ->
        Log.warnf "Temporary error making request [%s %s]: %s"
          meth uri exn >>= fun () ->
        failed_requests := (meth, uri, exn) :: !failed_requests;
        try_next ()

      | exn ->
        stop_flag := true;
        Log.fatalf ~exn "Unexpected error" >>= fun () ->
        log_request_failures !failed_requests >>= fun () ->
        Lwt.return None
    in

    Lwt_stream.from try_next
end