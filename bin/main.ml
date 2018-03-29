
open Lwt.Infix
open Printf
module Conf = Lib.Conf

let main () =
  let conf = Conf.(JW_source.of_string @@ read_file "config.json") in
  print_endline "Good morning, Starshine. The Earth says, \"Hello!\"";
  Printf.printf "JW API key: %s" @@ Conf.JW_source.key conf;

  let stream = Jw_client.get_videos_list_stream
    ~key:(Conf.JW_source.key conf)
    ~secret:(Conf.JW_source.secret conf)
    ~params:["result_limit", ["3"]]
    () in
  
  stream |> Lwt_stream.iter_s (fun ((offset, video) : int * Jw_client.video) ->
    printf "%d: %s" offset video.title;
    print_newline ();
    Lwt.return ()
  )

let () = 
  Random.self_init ();
  Lwt_main.run @@ main ()
