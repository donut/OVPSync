
open Lwt.Infix


module type Source = sig

  type t
  type offset

  val string_of_offset : offset -> string
  val offset_of_string : string -> offset

  val make_stream : ?offset:offset -> unit -> (offset * t) Lwt_stream.t
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
    let stream = Src.make_stream () in
    stream |> Lwt_stream.iter_p (fun (offset, src_item) ->
      let dest_item = Conf.dest_t_of_src_t src_item in
      Dest.save dest_item >>= fun _ ->
      Lwt.return ()
    )
end