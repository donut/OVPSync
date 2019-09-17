(** Managing sets of videos and returning them for processing. *)


(** Possible responses from {!step} that either return the current state of
    the set or the next video to process. *)
type step =
  | All_sets_finished
   (** All videos have been returned to be checked and none are still in 
       processing. Offset has been reset to 0, the next call to {!step} will
       start there. *)
  | New_set of ([`Offset of int] * [`Count of int])
    (** The first time getting a set of videos at the current offset. Next
        call to {!step} will return the first of these videos to check unless
        [`Count] is [0]. *)
  | Set_finished
    (** Finished the current set of videos at the current offset. Offset has
        been incremented. Next call to {!step} will get a new set of videos
        at the new offset. *)
  | New_in_current_set of int
    (** All videos at the current offset have been returned and none are still 
        processing, but [x] new videos have been added at the current offset
        that have not been checked previously. *)
  | Processing_to_check of int
    (** All videos at current offset have been checked, but there are still [x] 
        videos in processing that are now queued up to be checked. *)
  | Next of Jw_client.Platform.videos_list_video
    (** The next video to be checked. If we are waiting on external processes
        before we can finish checking it, pass it to {!add_to_processing} and
        it will be returned later to be checked again. *)


(** Config module to be passed to {!Make}. *)
module type Config = sig
  val base_jw_request_params : Jw_client.Platform.param list
  (** Any parameters that should be passed when requesting a set of videos
      from JW. *)
end


(** Interface to work through sets of videos. *)
module Make : functor 
  (Platform : Jw_client.Platform.Client)
  (Var_store : Sync.Variable_store)
  (Config : Config)
-> sig
  val step : unit -> step Lib.Result_lwt.t
  (** [step ()] returns the next video to be checked or runs the next step in
      handling the sets of videos to be checked. *)

  val add_to_processing : Jw_client.Platform.videos_list_video -> unit
  (** [add_to_processing vid] marks a [vid] as needing processing later on. 
      Once all videos in the current set have been returned at least once,
      this video will be returned again to be checked again. If it still is not
      ready, pass it to this function again. *)
end
