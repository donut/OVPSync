
open Lwt.Infix
open Printf
module Conf = Lib.Conf

module JW = Jw_client.Make(struct 
  let conf = Conf.(JW_source.of_string @@ read_file "config.json")

  let key = Conf.JW_source.key conf
  let secret = Conf.JW_source.secret conf
end)

module JW_src = Jw_source.Make(JW) 

let main () =
  print_endline "Good morning, Starshine. The Earth says, \"Hello!\"";

  let stream = JW_src.make_stream ~params:["result_limit", ["3"]] () in
  
  stream |> Lwt_stream.iter_p (fun ((offset, video) : int * Jw_client.video) ->
    printf "%d: %s" offset video.title;
    print_newline ();
    Lwt.return ()
  )

let () = 
  Random.self_init ();
  Lwt_main.run @@ main ()
