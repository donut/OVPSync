
open Lwt.Infix

open Sync_conf

exception Caqti_conn_error of string
exception Unexpected_arguments of string * string

let main () =
  let conf =
    let path = match Sys.argv with
      | [|_; path|] -> path
      | _ ->
        let args = Sys.argv |> Array.to_list |> String.concat "; " in
        raise @@ Unexpected_arguments
          ("The config path must be the only argument.", args)
    in
    Config.read_config path
  in

  let get_log_level = Config.log_level conf in

  Lwt_io.printl "Good morning, Starshine. The Earth says, \"Hello!\""
  >>= fun () ->

  let db_uri = Uri.of_string conf.db_conn in
  let pool = match Caqti_lwt.connect_pool db_uri with
  | Ok p -> p
  | Error e -> raise @@ Caqti_conn_error (Caqti_error.show e)
  in

  let m = make_jw_src pool get_log_level conf.jw_client conf.jw_source in
  let module JW_src = (val m : Jw_source.Made) in

  let m = make_rdb_dest pool get_log_level conf.rdb_dest in
  let module Dest = (val m : Rdb_dest.Made) in

  let module Log_synker = Logger.Make(struct
    let prefix = conf.sync.log_namespace
    let level = get_log_level conf.sync.log_level
  end) in

  let m = Make_sync.make 
    ~conf:conf.sync 
    ~ovp_name:("jw-" ^ conf.jw_client.key)
    (module JW_src) (module Dest) (module Log_synker)
  in
  let module Synker = (val m : Sync.Synchronizer) in

  Synker.sync () >>= fun () ->

  Caqti_lwt.Pool.drain pool

let () = 
  Random.self_init ();
  Lwt_main.run @@ main ()
