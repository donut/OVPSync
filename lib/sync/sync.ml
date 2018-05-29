
open Lwt.Infix


module type Variable_store = sig
  val set : string -> string -> unit Lwt.t
  val get : string -> ?default:string -> unit -> string Lwt.t
  val get_opt : string -> string option Lwt.t
  val get_like : string -> (string * string) list Lwt.t
  val delete : string -> unit Lwt.t
end


type logger_level =
  [ `Off
  | `Fatal
  | `Error
  | `Warn
  | `Info
  | `Debug
  | `Trace ]


module type Logger = sig
  val log : logger_level -> string -> unit Lwt.t
  val logf : logger_level -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  val fatal : ?exn:exn -> string -> unit Lwt.t
  val fatalf : ?exn:exn -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  val error : ?exn:exn -> string -> unit Lwt.t
  val errorf : ?exn:exn -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  val warn : ?exn:exn -> string -> unit Lwt.t
  val warnf : ?exn:exn -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  val info : string -> unit Lwt.t
  val infof : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val debug : string -> unit Lwt.t
  val debugf : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val trace : string -> unit Lwt.t
  val tracef : ('a, unit, string, unit Lwt.t) format4 -> 'a
end


module type Source = sig
  type t

  val make_stream : should_sync:(t -> bool Lwt.t) -> t Lwt_stream.t
  val cleanup : t -> unit Lwt.t
end


module type Destination = sig
  type t
  val save : t -> t Lwt.t
end


module type Config = sig
  type src_t
  type dest_t

  val dest_t_of_src_t : src_t -> dest_t
  val should_sync : (src_t -> bool Lwt.t)
end


module type Synchronizer = sig
  val sync : unit -> unit Lwt.t
end


module Make (Src : Source)
            (Dest : Destination)
            (Conf : Config with type dest_t = Dest.t and type src_t = Src.t)
: Synchronizer =
struct
  let sync () = 
    let stream = Src.make_stream ~should_sync:Conf.should_sync in
    stream |> Lwt_stream.iter_p (fun src_item ->
      let dest_item = Conf.dest_t_of_src_t src_item in
      Dest.save dest_item >>= fun _ ->
      Src.cleanup src_item
    )
end