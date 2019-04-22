
module Client = Client
module Video = Video
module Source = Source

module type Config = sig
  val db_pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t
  (* [files_path] Where to save video and thumbnail files to. *)
  val files_path : string
end

module Make (Log : Logger.Sig) (Conf: Config) : sig
  type t = Video.t
  
  (** [get_video ~ovp ~media_id] retrieves the video attached to the [ovp]  
      (Source.name) and [media_id] combination, if any. *)
  val get_video : ovp:string -> media_id:string -> t option Lwt.t
  val get_video_id_by_media_ids : (string * string) list -> int option Lwt.t

  val save : t -> t Lwt.t
end
