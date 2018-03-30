
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
  print_endline "Good morning, Starshine. The Earth says, \"Hello!\"";
  Synker.sync ()

let () = 
  Random.self_init ();
  Lwt_main.run @@ main ()
