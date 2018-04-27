
open Lwt.Infix
open Printf


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


let original_thumb_url media_id = 
  sprintf "https://cdn.jwplayer.com/thumbs/%s.jpg" media_id


module Make (Client : Jw_client.Platform.Client)
            (Var_store : Sync.Variable_store)
            (Log : Sync.Logger)
            (Conf : Config) =
struct

  type videos_video = Jw_client.Platform.videos_list_video
  type videos_conversion = Jw_client.Platform.videos_conversions_list_conversion
  type t = videos_video * string option * string option


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
    let%lwt { videos } = Client.videos_list ~params () in
    Lwt.return videos


  let sleep_if_few_left l =
    match List.length l with
    | 0 -> Lwt.return ()
    | count ->
      match 15 - count/3 with
      | s when s > 0 ->
        Log.infof "--> Few videos left processing; waiting %d seconds before checking again" s
        >>= fun () ->
        Lwt_unix.sleep (s |> float_of_int) 
      | _ ->  Lwt.return ()


  let get_status_and_passthrough media_id =
    let params = [("cache_break", [Random.bits () |> string_of_int])] in

    match%lwt Jw_client.Delivery.get_media media_id ~params () with
    | Some { playlist = media :: _ } -> 
      let passthrough = media.sources |> List.find_opt begin fun s -> 
        let open Jw_client.V2_media_body_t in
        match s.label with
        | None -> false
        | Some l -> String.lowercase_ascii l = "passthrough"
      end in
      Lwt.return (true, passthrough)

    | Some { playlist = [] } -> Lwt.return (true, None)
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


  let cleanup_by_media_id media_id ?changed () =
    Log.infof "Undoing changes to [%s]" media_id >>= fun () ->
    let%lwt { expires; passthrough } = match changed with 
    | None -> get_changed media_id
    | Some c -> Lwt.return c 
    in

    begin match expires with 
    | None -> Lwt.return ()
    | Some expires_date ->
      Log.infof "[%s] Undoing publish" media_id >>= fun () ->
      match%lwt Client.videos_show media_id with
      | None -> Lwt.return ()
      | Some { video } ->
        (* Make sure we have the latest expires date in case it was set
         * outside of this program. *)
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
      Log.infof "[%s] Deleting passthrough conversion" media_id >>= fun () ->
      try%lwt Client.delete_conversion_by_name media_id "passthrough" with
      (* Likely deleted outside this program. *)
      | Not_found -> Lwt.return () 
    else
      Lwt.return ()
    end >>= fun () ->

    clear_changed media_id >>= fun () ->
    Log.infof "[%s] Undid all changes" media_id


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
      BatOption.is_none @@ List.find_opt ((=) media_id) exclude)
    |> List.map (fun (media_id, changes) ->
      (media_id, Modified.of_string changes))
    |> List.filter (fun ((_, { timestamp }) : string * Modified.t) ->
        (now - timestamp) > min_age)
    |> Lwt_list.iter_p (fun (media_id, changed) ->
      cleanup_by_media_id media_id ~changed ())


  let refresh_current_videos_set ~returned =
    let%lwt offset = Var_store.get "request_offset" ~default:"0" () in
    let%lwt vids = get_set (offset |> int_of_string) in
    let to_check = vids 
      |> List.filter (fun (v : videos_video) ->
        BatOption.is_none @@ List.find_opt
          (fun (r : videos_video) -> r.key = v.key) returned)
    in

    Lwt.return (vids, to_check)


  let prepare_video_for_sync (vid : videos_video) ~published ~passthrough =
    let%lwt prev_changes = get_changed vid.key in
    let now = Unix.time () |> int_of_float in

    begin match published, vid.expires_date with
    | true, _ -> Lwt.return prev_changes
    | false, None ->
      Log.infof "[%s] Waiting on publish." vid.key >>= fun () ->
      Lwt.return prev_changes
    | false, Some e when e > now ->
      Log.infof "[%s] Waiting on publish." vid.key >>= fun () ->
      Lwt.return prev_changes
    | false, Some _ ->
      Log.infof "[%s] Not published; publishing..." vid.key
        >>= fun () ->
      let changes =
        { prev_changes with expires = vid.expires_date } in
      set_changed vid.key changes >>= fun () ->
      publish_video vid >>= fun () ->
      Lwt.return changes
    end >>= fun changes ->

    begin match passthrough with
    | Some _ -> Lwt.return ()
    | None ->
      begin if changes.passthrough then
        let%lwt { conversions }
          = Client.videos_conversions_list vid.key
        in
        let passthrough = conversions
          |> List.find_opt (fun (c : videos_conversion) ->
            String.lowercase_ascii c.template.name = "passthrough")
        in
        match passthrough with
        | None -> Lwt.return true
        | Some { status = `Failed; key = k } ->
          Log.errorf "[%s] Passthrough conversion creation failed"  
            vid.key >>= fun () ->
          Client.videos_conversions_delete k >>= fun () ->
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
          Client.create_conversion_by_name vid.key "passthrough"
        else
          Log.infof "[%s] Waiting on passthrough." vid.key
      end


let make_stream ~(should_sync : (t -> bool Lwt.t)) : t Lwt_stream.t =
  let current_videos_set = ref [] in
  let videos_to_check = ref [] in
  let processing_videos = ref [] in

  let rec next () =
    begin match !videos_to_check, !processing_videos with
    | [], [] ->
      (* Check for new videos at the current offset in case more were 
        * added (or some removed, pushing more into the current offset) in 
        * the time it took to process the current set *)
      let%lwt refreshed = match !current_videos_set with
      | [] -> Lwt.return ([], []) 
      | returned -> refresh_current_videos_set ~returned
      in

      begin match refreshed with
      | _, [] ->
        Log.info "Getting next set..." >>= fun () ->
        let%lwt offset = Var_store.get "request_offset" ~default:"0" () in
        let new_offset =
          (offset |> int_of_string) + (List.length !current_videos_set) in
        Var_store.set "request_offset" (new_offset |> string_of_int)
          >>= fun () ->
        Log.infof "--> offset: %d" new_offset >>= fun () ->

        let%lwt vids = get_set new_offset in

        current_videos_set := vids;
        videos_to_check := vids;
        Log.info "--> done!" >>= fun () ->

        Log.info "Cleaning up old changes..." >>= fun () ->
        let exclude = !videos_to_check
          |> List.map (fun (v : videos_video) -> v.key)
        in
        cleanup_old_changes ~exclude ~min_age:(12 * 60 * 60) ()

      | refreshed_set, refreshed_to_check ->
        Log.info "Returned all vidoes in current set, but more were added at the current offset during that time. Processing those..." 
          >>= fun () ->
        current_videos_set := refreshed_set;
        videos_to_check := refreshed_to_check;
        Lwt.return ()
      end

    | [], _ -> 
      Log.info "List of videos to check exhausted. Refreshing data of those still in processing and setting them up to be checked again..."
        >>= fun () ->
      (* Be sure we grab any updates that happened outside this program 
        * during the last pass. *)
      let returned_videos = !current_videos_set
        |> List.filter (fun (c : videos_video) ->
          BatOption.is_none @@ List.find_opt
            (fun (p : videos_video) -> p.key = c.key) !processing_videos)
      in
      let%lwt (refreshed_set, refreshed_to_check) =
        refresh_current_videos_set ~returned:returned_videos 
      in

      current_videos_set := refreshed_set;
      videos_to_check := refreshed_to_check;
      processing_videos := [];

      sleep_if_few_left !videos_to_check 

    | _, _
      -> Lwt.return ()
    end >>= fun () ->

    match !videos_to_check with
    | [] -> 
      Log.info "Reached the end of all videos." >>= fun () ->
      Lwt.return None

    | vid :: tl ->
      Log.infof "Checking video [%s] %s" vid.key vid.title >>= fun () ->
      videos_to_check := tl;

      let%lwt sync_needed = should_sync (vid, None, None) in
      match sync_needed, vid.status, vid.sourcetype with
      | false, _, _ ->
        Log.infof "[%s] No need to sync. NEXT!" vid.key >>= fun () ->
        next ()
      | true, (`Created | `Processing | `Updating | `Failed), `File
      | true, _, `URL ->
        Log.infof "[%s] has URL source or non-ready status. RETURNING!"
          vid.key >>= fun () ->
        let thumb = original_thumb_url vid.key in
        Lwt.return (Some (vid, vid.sourceurl, Some thumb))
      | true, `Ready, `File ->
        Log.infof "[%s] Getting publish and passthrough status." vid.key
          >>= fun () ->

        match%lwt get_status_and_passthrough vid.key with
        | true, Some p ->
          Log.infof "[%s] Video is published and has passthrough. RETURNING!"
            vid.key >>= fun () ->
          (* @todo Run persistent storage cleanup if [to_check] and 
            * [processing] are empty *)
          let thumb = original_thumb_url vid.key in
          Lwt.return (Some (vid, Some p.file, Some thumb))

        | published, passthrough ->
          match%lwt prepare_video_for_sync vid ~published ~passthrough with
          | exception Not_found -> 
            Log.infof "[%s] Looks like this video no longer exists. NEXT!"
              vid.key >>= fun () ->
            next ()
          | () ->
            Log.infof "[%s] Adding to processing list. NEXT!" vid.key
              >>= fun () ->
            processing_videos := vid :: !processing_videos;
            next ()

  in

  Lwt_stream.from begin fun () ->
    try%lwt next () with
    | Jw_client.Util.Unexpected_response_status (status, headers, body) ->
      Log.fatalf
        "Unexpected HTTP response\n\
          --> Status: %s\n\n\
          --> Headers <--\n%s\n\n\
          --> Body <--\n%s\n"
        status headers body >>= fun () ->
      Lwt.return None
    | exn ->
      Log.fatalf ~exn "Unexpected error" >>= fun () ->
      Lwt.return None
  end

end