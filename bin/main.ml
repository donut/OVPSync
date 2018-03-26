
open Lwt.Infix
module Conf = Lib.Conf

let main () =
  let conf = Conf.(JW_source.of_string @@ read_file "config.json") in
  print_endline "Good morning, Starshine. The Earth says, \"Hello!\"";
  Printf.printf "JW API key: %s" @@ Conf.JW_source.key conf;

  Jw_client.call
    ~key:(Conf.JW_source.key conf) ~secret:(Conf.JW_source.secret conf)
    "/videos/list" 
    ()


let () = 
  Random.self_init ();
  Lwt_main.run @@ main ()
