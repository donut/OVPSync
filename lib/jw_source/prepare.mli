(** Makes changes to videos in preparation for sync and manages undoing those
    changes post-sync or when sync is not necessary. *)


type t = Jw_client.Platform.videos_list_video
       * (string * int option * int option) option
       * string option
(** [video * (file_uri * video_width * video_height) * thumbnail_uri] represents
    a prepared video. *)


(** Why a video is considered prepared *)
type prepared = 
  | Has_non_ready_status
    (** The video has a non-ready status. This typically means that the file
        is still being transcoded at the source. It can also mean that that
        process has failed. In any case, we can still sync what we know about
        it now and sync the file details at a later pass. *)
  | Source_is_URL
    (** The file associated with this video is not hosted by the source, but
        at the specified source URL. We don't need to do prep in order to 
        sync. *)
  | Published_with_passthrough
    (** The video is published and has a passthrough conversion. A passthrough
        conversion is a way to access the original video file on JW's servers.
        To access that passthrough, the video must be published. *)


(** The preparation status of a video passted to {!video}. *)
type status =
  | No_need_to_sync
    (** The passed [should_sync] function determined there was no need to sync
        this video. *)
  | Missing
    (** No longer exists at JW. Likely was deleted in the time between getting
        the set of videos and checking on this one. *)
  | Processing
    (** Waiting on external processing before being able to sync this video. *)
  | Prepared of t * prepared
    (** Video is prepared. [t] is the prepared version and [prepared] is why it
        is considered ready for sync. *)


(** Configuration to pass to the {!Make} functor. *)
module type Config = sig
  val backup_expires_field : string
  (** In case a video's [expires_date] field is changed in preparation for sync,
      the old value is saved to this custom field of the video on JW. This field
      only serves as a backup in case the records his app keeps are lost. *)

  val temp_pub_tag : string
  (** The tag to apply to videos on JW when they are temporarily published for
      the purpose of syncing. *)
end


(** Interface to prepare videos for sync. *)
module Make : functor
  (Platform : Jw_client.Platform.Client)
  (Var_store : Sync.Variable_store)
  (Log : Logger.Sig) 
  (Config : Config)
-> sig
  val cleanup_by_media_id
    : ?changed : Changes.Record.t -> string -> unit Lib.Lwt_result.t
  (** [cleanup_by_media_id ?changed media_id] undos changes made to the video
      on JW represented by [media_id]. Passing [changed] allows skipping a 
      lookup in [Var_store]. *)

  val cleanup_old_changes 
    : exclude : string list -> ?min_age : int -> unit -> unit Lwt.t
  (** [cleanup_old_changes exclude ?min_age ()] undos all recorded changes to
      JW videos that were made at least [min_age] seconds ago (defaults to 0)
      except for videos whose media ID is listed in [exclude]. *)

  val video 
     : Jw_client.Platform.videos_list_video 
    -> should_sync : (t -> bool Lwt.t)
    -> status Lib.Lwt_result.t
  (** [video vid ~should_sync] prepares [vid] for sync unless passing it to
      [should_sync] returns [false], returning the status of the video if it's
      not ready for sync yet, or the video along with metadata regarding its
      video file and thumbnail if it is properly prepared for sync. *)
end