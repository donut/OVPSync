
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
end


module type Config = sig
  val params : Jw_client.Platform.param list
  val temp_pub_tag : string
end


let original_thumb_url media_id = 
  sprintf "https://cdn.jwplayer.com/thumbs/%s.jpg" media_id


module Make (Client : Jw_client.Platform.Client)
            (Var_store : Sync.Variable_store)
            (Log : Sync.Logger)
            (Conf : Config) =
struct

  type t = Jw_client.Platform.videos_list_video * string * string

  let changed_video_key media_id = "video-changed-" ^ media_id

  let get_changed media_id =
    let key = changed_video_key media_id in
    Var_store.get_opt key >>= function
    | None   -> Lwt.return @@ Modified.make ~passthrough:false ()
    | Some v -> Lwt.return @@ Modified.t_of_sexp @@ Sexplib.Sexp.of_string v

  let set_changed media_id changes =
    let key = changed_video_key media_id in
    let value = changes |> Modified.to_string in
    Var_store.set key value

  let make_stream ~(should_sync : (t -> bool Lwt.t)) : t Lwt_stream.t =
    (* 0: Check if video has been updated since last synced to dest *)
    (* 1: Check if video is published and has passthrough *)
    (* 2: If not, add to runtime and permanent list for revisiting and clean up
          later and make API calls to publish and/or add passthrough. *)
    (* 3: Loop through current set all have been passed on to dest. *)
    (* 4: Clean up those confirmed synced to destination, via 
          [cleanup t -> unit Lwt.t] *)
    (* 6: At the end of each set, check permanent list to clean up any times
          not in current list. *)
    (* 7: Move to next set. *)
    let request_offset = ref 0 in
    let current_videos_set = ref [] in
    let videos_to_check = ref [] in
    let processing_videos = ref [] in

    let next_set offset =
      let params = Jw_client.Util.merge_params
        Conf.params
        [ "result_offset", [offset |> string_of_int];
          "statuses_filter", ["ready"] ]
      in
      Client.get_videos_list ~params () >>= fun body ->
      Lwt.return body.videos
    in

    let rec next () =
      begin match !videos_to_check, !processing_videos with
      | [], [] ->
        Log.info "Getting next set..." >>= fun () ->
        request_offset := !request_offset + (List.length !current_videos_set);
        Log.infof "--> offset: %d" !request_offset >>= fun () ->
        next_set !request_offset >>= fun vids ->

        Log.info "--> done!" >>= fun () ->
        current_videos_set := vids;
        videos_to_check := vids;
        Lwt.return ()

      | [], _ -> 
        (* @todo Is this the place to refresh the current set in case of
         * changes? *)
        Log.info 
          "`videos_to_check` exhausted; reloading with `processing_vidoes`"
        >>= fun () ->
        (* Lwt_unix.sleep 10. >>= fun () -> *)
        videos_to_check := List.rev !processing_videos;
        processing_videos := [];
        let count = List.length !videos_to_check in
        begin match 15 - count/3 with
        | s when s > 0 ->
          Log.infof "--> Few videos left processing; waiting %d seconds before checking again" s
          >>= fun () ->
          Lwt_unix.sleep (s |> float_of_int) 
        | _ ->  Lwt.return ()
        end

      | _, _
        -> Lwt.return ()
      end

      >>= fun () ->
      match !videos_to_check with
      | [] -> 
        Log.info "Reached the end of all videos." >>= fun () ->
        Lwt.return None
      | vid :: tl ->
        Log.infof "Checking video [%s] %s" vid.key vid.title >>= fun () ->
        videos_to_check := tl;

        should_sync (vid, "", "") >>= function
        | false ->
          Log.infof "[%s] No need to sync. NEXT!" vid.key
          >>= fun () ->
          next ()
        | true ->
          Log.infof "[%s] Getting publish and passthrough status." vid.key
          >>= fun () ->
          let params = [("cache_break", [Random.bits () |> string_of_int])] in
          Jw_client.Delivery.get_media vid.key ~params () >>= begin function
          | Some { playlist = media :: _ } -> 
            let passthrough = media.sources |> List.find_opt (fun s -> 
              let open Jw_client.V2_media_body_t in
              match s.label with
              | None -> false
              | Some l -> String.lowercase_ascii l = "passthrough")
            in
            Lwt.return (true, passthrough)

          | Some { playlist = [] } ->
            Lwt.return (true, None)
          | None ->
            Lwt.return (false, None)
          end
          
          >>= function 
          | true, Some p ->
            Log.infof "[%s] Video is published and has passthrough. RETURNING!"
              vid.key
            >>= fun () ->
            (* @todo Run persistent storage cleanup if [to_check] and 
              * [processing] are empty *)
            let thumb = original_thumb_url vid.key in
            Lwt.return (Some (vid, p.file, thumb))

          | published, passthrough ->
            get_changed vid.key >>= fun previous_changes ->
            begin match published, previous_changes.expires with
            | true, _ -> Lwt.return None
            | false, Some _ ->
              Log.infof "[%s] Waiting on publish." vid.key >>= fun () ->
              Lwt.return (Some previous_changes)
            | false, None ->
              Log.infof "[%s] Not published; publishing..." vid.key
              >>= fun () ->
              let changes = Modified.make
                ?expires:vid.expires_date
                ~passthrough:previous_changes.passthrough
                ()
              in
              set_changed vid.key changes >>= fun () ->
              let tags = vid.tags
                |> String.split_on_char ','
                |> List.map String.trim
                |> (fun l -> Conf.temp_pub_tag :: l)
                |> String.concat ", "
              in
              let params = [("expires_date", [""]); ("tags", [tags])] in
              Client.videos_update vid.key params >>= fun () ->
              Lwt.return (Some changes)
            end

            >>= fun changes ->
            begin match passthrough, previous_changes.passthrough with
            | Some _, _   -> Lwt.return ()
            | None, true  -> Log.infof "[%s] Waiting on passthrough." vid.key
            | None, false ->
              Log.infof "[%s] No passthrough; creating..." vid.key >>= fun () ->
              let changes' = match changes with 
              | Some c -> Modified.make ?expires:c.expires ~passthrough:true ()
              | None -> Modified.make ~passthrough:true ()
              in
              set_changed vid.key changes' >>= fun () ->
              Client.create_conversion_by_name vid.key "passthrough"
            end
            
            >>= fun () ->
            Log.infof "[%s] Adding to processing list. NEXT!" vid.key
            >>= fun () ->
            processing_videos := vid :: !processing_videos;
            next ()
    in

    Lwt_stream.from next


  let cleanup t = Lwt.return ()

end