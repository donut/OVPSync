

(** [make conf ovp_name Src Dest Log] builds a config module and passes it to
    the [Sync.Make] functor. *)
val make : 
    conf:Lib.Conf.Sync.t -> ovp_name:string 
    -> (module Jw_source.Made) -> (module Rdb_dest.Made) -> (module Logger.Sig)
    -> (module Sync.Synchronizer)