

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


module Make : functor
  (Client : Jw_client.Platform.Client)
  (Var_store : Sync.Variable_store)
  (Log : Logger.Sig)
  (Conf : Config) 
  -> Made