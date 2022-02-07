
open Lwt.Infix


module type Variable_store = sig
  val set : string -> string -> unit Lwt.t
  val get : string -> ?default:string -> unit -> string Lwt.t
  val get_opt : string -> string option Lwt.t
  val get_like : string -> (string * string) list Lwt.t
  val delete : string -> unit Lwt.t
end


module type Source = sig
  type t

  val make_stream
    : should_sync:(t -> bool Lwt.t) -> stop_flag:(bool ref) -> t Lwt_stream.t
  val cleanup : t -> unit Lib.Result_lwt.t
  val final_cleanup : unit -> unit Lwt.t
end


module type Destination = sig
  type t
  val save : t -> t Lwt.t
end


module type Config = sig
  type src_t
  type dest_t

  val max_threads : int
  val loop_infinitely : bool
  val dest_t_of_src_t : src_t -> dest_t Lwt.t
  val should_sync : (src_t -> bool Lwt.t)
end


module type Synchronizer = sig
  val sync : unit -> unit Lwt.t
end


module Make (Src : Source)
            (Dest : Destination)
            (Log : Logger.Sig)
            (Conf : Config with type dest_t = Dest.t and type src_t = Src.t)
: Synchronizer =
struct
  let rec sync () = 
    (* This stop flag instructs the stream to stop streaming early.
       When using the [_p] (parallelized) variant of [Lwt_stream.iter], 
       exceptions raised in the passed function will be ignored and it will 
       continue to iterate on the stream. This allows us to stop on 
       exceptions. *)
    let stop_flag = ref false in
    let should_sync, max_concurrency = Conf.(should_sync, max_threads) in

    let stream = Src.make_stream ~should_sync ~stop_flag in
    (* Limit the number of threads to avoid [Unix.EINVAL] exceptions.
       @see https://github.com/ocsigen/lwt/issues/222 *)

    let%lwt () = 
      stream |> Lwt_stream.iter_n ~max_concurrency begin fun src_item ->
        let%lwt () = 
          try%lwt
            src_item
            |> Conf.dest_t_of_src_t
            >>= Dest.save
            >|= ignore

          with | exn ->
            let%lwt () = Log.fatal ~exn
              "Failed saving item to destination. STOPPING!" in
            stop_flag := true;
            Lwt.return ()
        in

        match%lwt Src.cleanup src_item with
        | Error exn ->
          Log.error ~exn "Failed cleaning up item."

        | Ok () ->
          Lwt.return ()
      end
    in

    if !stop_flag then Lwt.return ()
    else
    
    let%lwt () = Src.final_cleanup () in
    
    let%lwt () = Log.info
      ( "\n###############################\n"
      ^   "         FINISHED RUN          \n"
      ^   "###############################" )
    in

    if Conf.loop_infinitely 
    then sync ()
    else Lwt.return ()

end