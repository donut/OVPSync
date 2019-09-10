(** Managing records of changes that were made to videos to facilitate syncing.
    *)

module Record : sig
  (** Represents a set of changes made to a single video. *)
  type t =
    { timestamp : int
    ; expires : int option
    ; passthrough : bool }
end

val set_record 
  : (module Sync.Variable_store) -> string -> Record.t -> unit Lwt.t
(** [set_record var_store media_id record] adds or updates record with
    [media_id] in [var_store] with values in [record]. *)

val get_record : (module Sync.Variable_store) -> string -> Record.t Lwt.t
(** [get_record var_store media_id] retreives a record from [var_store]
    with [media_id] or creates a new record for [media_id] with no changes
    recorded and not saved to [var_store]. *)

val get_all_records 
   : (module Sync.Variable_store) 
  -> ?except : string list 
  -> ?min_age : int 
  -> unit
  -> (string * Record.t) list Lwt.t
(** [get_all_records ?except ?min_age var_store] returns all records from 
    [var_store] except those with media_ids listed in [except] or whose age
    is less than [min_age] in seconds. *)

val clear_record : (module Sync.Variable_store) -> string -> unit Lwt.t
(** [clear_record var_store media_id] deletes record identified by [media_id]
    in [var_store] if it exists. *)