(** For interacting with file stores where file stores are just absoluate paths
    to locations to store files.  This solves the problem of needing to use 
    multiple drives/locations to store the video and thumbnail files. *)


type pick = 
  { path : string option
  ; available_space : int
  ; required_space : int
  ; checked : (string * int) list }
(** Represents the chosen file store as well as the data surrounding that 
    choice. If {!path} is {!None} that means that no suitable store was
    found. {!checked} will have a list of stores that weren't chosen and the
    available space in each. *)


val pick 
  : ?required_space:int 
  -> string list 
  -> (pick, Logger.Level.t * exn option * string) result
(** [pick ?required_space stores] checks each file store path in [stores] to
    see if they have the the [requied_space]. Returns a {!pick} with a with
    the result. *)

val make_local_uri : string -> string -> string
(** [make_local_uri rel_path filename] generates a URI that could be relative
    to any local file store. *)

val is_local_uri : Uri.t -> bool
(** [is_local_uri uri] returns {!true} if [uri] is local. This does not
    guarantee that the file exists locally, only that [uri] is properly
    formed. *)

val abs_path_of_local_uri : string -> Uri.t -> string
(** [abs_path_of_local_uri store uri] generates the absolute path of the local
    URI for the given [store]. *)

val store_of_local_uri : string list -> Uri.t -> string option Lwt.t
(** [store_of_local_uri stores uri] searches each file store in [stores] for
    a file matching [uri]. If [uri] is not a local URI (see {!is_local_uri})
    or the URI does not match any files in any of the stores, {!None} is 
    returned. Otherwise, the first store searched with a matching file is
    returned. *)

val unlink_uri_if_found_on_stores : string list -> Uri.t -> unit Lwt.t
(** [unlink_uri_if_found_on_stores stores uri] deletes the first file found
    in [stores] that matches [uri] if URI is local (see {!is_local_uri}). *)


module File : sig
  (** Handle individual files in a file store. *)

  type t =
    { store : string
    ; uri : Uri.t
    ; size : int }
  (** Represents an individual file in a file store. {!uri} is the local URI
      of the file. {!size} is in bytes. *)

  val of_local_uri : string list -> Uri.t -> t option Lwt.t
  (** [of_local_uri stores uri] returns a {!t} of the file represented by [uri]
      if [uri] is local (see {!is_local_uri}) and exists in one of the file 
      stores passed in [stores]. Searches [stores] in order and returnts the
      first match. *)
end