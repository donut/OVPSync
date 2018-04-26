
open Lwt.Infix
open Printf

module Conf = Lib.Conf




let main () =
  let conf = Conf.(JW_source.of_string @@ read_file "config.json") in

  Lwt_io.printl "Good morning, Starshine. The Earth says, \"Hello!\""
  >>= fun () ->

  let db_uri = Uri.of_string "mariadb://ovp_sync:abc123@localhost/ovp_sync" in
  Lwt.return @@ Caqti_lwt.connect_pool db_uri >>=
  Caqti_lwt.or_fail >>= fun pool ->

  let module JW = Jw_client.Platform.Make(struct 
    let key = Conf.JW_source.key conf
    let secret = Conf.JW_source.secret conf
    let rate_limit_to_leave = Conf.JW_source.rate_limit_to_leave conf
  end) in

  let module JW_var_store = Variable_store.Make(struct
    let db_pool = pool
    let namespace = "JWsrc-" ^ Conf.JW_source.key conf
  end) in

  let module Log_jw = Logger.Make(struct
    let prefix = "JW_src"
    let level = `Trace
  end) in

  let module JW_src = Jw_source.Make(JW)(JW_var_store)(Log_jw)(struct
    let params = ["result_limit", ["10"]] 
    let temp_pub_tag = "Temporarily Published"
    let backup_expires_field = "ovp_sync.backup_expires_date"
  end) in

  let module Log_pdest = Logger.Make(struct
    let prefix = "P_dest"
    let level = `Debug
  end) in

  let module Printer_dest = struct
    type t = string

    let save message = 
      Log_pdest.info message >>= fun () ->
      Lwt.return message
  end in

  let module Synker = Sync.Make(JW_src)(Printer_dest)(struct
    type src_t = JW_src.t
    type dest_t = Printer_dest.t

    let dest_t_of_src_t ((item, file, thumb) : src_t) = item.title
    let should_sync _ = Lwt.return true
  end) in

  Lwt_io.printl "test" >>=
  Synker.sync

let () = 
  Random.self_init ();
  Lwt_main.run @@ main ()
