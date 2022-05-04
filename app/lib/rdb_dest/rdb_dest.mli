
module Client = Client
module Video = Video
module Source = Source

val is_local_uri : Uri.t -> bool
(** [is_local_uri uri] checks if [uri] looks to be a local or not. Does not
    check if URI actually references a real file. See 
    {!File_store.is_local_uri}. *)

module type Config = sig
  (** Configuration to be passed into {!Make}. *)

  val db_pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t

  val file_stores : string list
  (** List of absolute paths to locations on the file system to save files to.
      Every time a file needs to save, they'll be checked for space in order
      and the first one with free space will be chosen. *)
end

module type Made = sig
  type t = Video.t
  
  val get_video : ovp:string -> media_id:string -> t option Lwt.t
  (** [get_video ~ovp ~media_id] retrieves the video attached to the [ovp]  
      {!Source.name} and [media_id] combination, if any. *)

  val get_video_id_by_media_ids : (string * string) list -> int option Lwt.t
  (** [get_video_id_by_media_ids media_ids] returns the DB ID of the first 
      video with a source that matches one of those in [media_ids]. Each item 
      in [media_ids] is a tuple of `(<OVP ID>, <media ID>)`. *)

  val save : t -> t Lwt.t
  (** [save t] saves [t] to the database and associated files to the file 
      system. *)
end

module Make : functor (Log : Logger.Sig) (Conf : Config) -> Made
