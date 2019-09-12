
open Base
open Printf

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


module Make (Client : Jw_client.Platform.Client)
            (Var_store : Sync.Variable_store)
            (Log : Logger.Sig)
            (Conf : Config)
            : Made =
struct
  module Videos_set = Videos_set.Make (Client) (Var_store) (struct
    let base_jw_request_params = Conf.params
  end)


  module Prep = Prepare.Make (Client) (Var_store) (Log) (struct
    let backup_expires_field = Conf.backup_expires_field
    let temp_pub_tag = Conf.temp_pub_tag
  end)


  type t = Prepare.t

    
  let sleep_if_few_left count =
    if count = 0 then Lwt.return ()
    else 

    match 15 - count/3 with
    | s when s > 0 ->
      let%lwt () = Log.infof
        "--> Only %d videos left processing; waiting %d seconds before checking again."
        count s
      in
      Lwt_unix.sleep (s |> Float.of_int) 

    | _ ->
      Lwt.return ()


  let cleanup (({ key; _ }, _, _) : t) = Prep.cleanup_by_media_id key


  let final_cleanup () = Prep.cleanup_old_changes ~exclude:[] ()


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

      let%lwt () = Log.debugf "Running next videos set step..." in

      match%bind Videos_set.step () with
      | All_sets_finished ->
        let%lwt () = log_request_failures !failed_requests in
        let%lwt () = Log.info "Processed all videos at source." in
        Lwt_result.return None

      | New_set (`Offset offset, `Count count) ->
        let%lwt () =
          Log.infof "Got new set of %d videos at offset %d." count offset in
        next ()

      | Set_finished ->
        let%lwt () = Log.debug "Finished all videos at current offset." in
        let%lwt () = Log.debug "Cleaning up old changes." in
        let%lwt () =
          Prep.cleanup_old_changes ~exclude:[] ~min_age:(12 * 60 * 60) () in
        next ()

      | New_in_current_set count ->
        let%lwt () = Log.infof
          "Returned all vidoes in current set, but %d more were added at the current offset during that time. Processing those." 
          count
        in
        next ()

      | Processing_to_check count ->
        let%lwt () = Log.debugf
          "Checked all videos at current offset, but still waiting on %d vidoes marked as processing."
          count
        in
        let%lwt () = sleep_if_few_left count in
        next ()

      | Next ({ key; title; _ } as vid) ->
        let%lwt () = Log.debugf "Checking [%s: %s]" key title in

        begin match%bind Prep.video ~should_sync vid with 
        | No_need_to_sync ->
          let%lwt () = Log.infof "[%s] No need to sync. NEXT!" vid.key in
          next ()

        | Missing -> 
          let%lwt () = Log.infof
            "[%s] Looks like this video no longer exists. NEXT!" key in
          next ()
        
        | Processing ->
          let%lwt () = Log.debugf "[%s] Adding to processing list. NEXT!" key in
          let () = Videos_set.add_to_processing vid in
          (* This video isn't ready to be returned yet, take the next
             video in the list. This video will get looked at again once
             we've gone through every video in the current list to check. *)
          next ()

        | Prepared (t, how) ->
          let message =
            match how with
            | Has_non_ready_status -> "has non-ready status."
            | Source_is_URL -> "has URL source."
            | Published_with_passthrough -> "is published and has passthrough."
          in
          let%lwt () = Log.infof "[%s] %s RETURNING!" key message in

          Lwt_result.return @@ Some t
        end
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
        let%lwt () = Log.errorf
          "Unexpected HTTP response\n\
            --> Request: [%s %s]\n\n\
            --> Response <--\n%s\n\n"
          method_ path response
        in
        let%lwt () = log_request_failures !failed_requests in
        try_next ()

      | Error (Jw_client.Exn.Temporary_error (meth, uri, exn)) ->
        let%lwt () = Log.warnf
          "Temporary error making request [%s %s]: %s"
          meth uri exn
        in
        let () = failed_requests := (meth, uri, exn) :: !failed_requests in
        try_next ()

      | Error exn ->
        let () = stop_flag := true in
        let%lwt () = Log.errorf ~exn "Unexpected error" in
        let%lwt () = log_request_failures !failed_requests in
        try_next ()

      | Ok video ->
        Lwt.return video
    in

    Lwt_stream.from try_next
end