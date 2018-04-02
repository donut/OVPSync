
open Lwt.Infix
open Printf

module Conf = Lib.Conf


module JW = Jw_client.Make(struct 
  let conf = Conf.(JW_source.of_string @@ read_file "config.json")
  let key = Conf.JW_source.key conf
  let secret = Conf.JW_source.secret conf
end)


module JW_src = Jw_source.Make(JW)(struct
  let params = ["result_limit", ["3"]] 
end) 


module Printer_dest = struct

  type t = string

  let save message = 
    print_endline message;
    Lwt.return message

end


module Synker = Sync.Make(JW_src)(Printer_dest)(struct
  type src_t = JW_src.t
  type dest_t = Printer_dest.t

  let dest_t_of_src_t (src_item : src_t) = src_item.title
end)


let main () =
  Lwt_io.printl "Good morning, Starshine. The Earth says, \"Hello!\""
  >>= fun () ->
  let db_uri = "mariadb://ovp_sync:abc123@localhost/ovp_sync" in
  Rdb_dest.Client.connect (Uri.of_string db_uri) >>=
  Caqti_lwt.or_fail >>= fun dbc ->
  let module DB = (val dbc : Caqti_lwt.CONNECTION) in
  print_endline "Connected?";
  let get = Caqti_request.find
    Caqti_type.unit Caqti_type.(tup4 int string string string)
    "SELECT * FROM setting" in
  DB.find get () >>= (function
    | Ok (id, ns, name, value) ->
      Lwt_io.printl (ns ^ "." ^ name ^ ": " ^ value)
    | Error err -> 
      Lwt_io.printl (Caqti_error.show err)
  ) >>= fun () ->
  Lwt_io.printl "test" >>=
  Synker.sync

let () = 
  Random.self_init ();
  Lwt_main.run @@ main ()
