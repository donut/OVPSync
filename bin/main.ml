
open Lwt.Infix
open Printf

module Conf = Lib.Conf



module JW = Jw_client.Make(struct 
  let conf = Conf.(JW_source.of_string @@ read_file "config.json")
  let key = Conf.JW_source.key conf
  let secret = Conf.JW_source.secret conf
end)

module Log_jw = Logger.Make(struct
  let prefix = "JW_src"
  let level = `Debug
end)

module JW_src = Jw_source.Make(JW)(Log_jw)(struct
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
  let db_uri = Uri.of_string "mariadb://ovp_sync:abc123@localhost/ovp_sync" in
  Lwt.return @@ Caqti_lwt.connect_pool db_uri >>=
  Caqti_lwt.or_fail >>= fun pool ->
  print_endline "Connected?";

  let module Var_store = Variable_store.Make(struct
    let db_pool = pool
    let namespace = "t"
  end) in

  Var_store.set "dingo" "bingo" >>= fun () ->
  Var_store.set "goomba" "koopa" >>= fun () ->
  Var_store.set "video-1234" "boop" >>= fun () ->
  Var_store.set "video-4321" "poob" >>= fun () ->
  Var_store.set "video-3421" "obpo" >>= fun () ->

  Var_store.get "dingo" () >>= fun v ->
  Lwt_io.printl ("got dingo: " ^ v) >>= fun () ->

  Var_store.set "dingo" "rango" >>= fun () ->

  Var_store.get "dingo" () >>= fun v ->
  Lwt_io.printl ("got dingo: " ^ v) >>= fun () ->

  Var_store.get "jango" ~default:"fett" () >>= fun v ->
  Lwt_io.printl ("got jango: " ^ v) >>= fun () ->

  Var_store.get_opt "goomba" >>= begin function 
    | Some v -> Lwt_io.printl ("got goomba: " ^ v)
    | None -> Lwt_io.printl "didn't get goomba :"
  end >>= fun () ->

  Var_store.get_like "video-%" >>= fun vars ->
  print_endline "Found vars:";
  vars |> List.iter (fun (k, v) -> print_endline (k ^ ": " ^ v));

  ["dingo"; "goomba"; "video-1234"; "video-4321"; "video-3421"]
  |> List.map (fun k -> Var_store.delete k)
  |> Lwt.join
  >>= fun () ->

  Lwt.catch
    (fun () ->
      Var_store.get "goomba" () >>= fun v ->
      Lwt_io.printl ("This should not exist: " ^ v))
    (function
      | Not_found -> Lwt_io.printl "goomba not found"
      | e -> raise e)
  >>= fun () ->

  Lwt_io.printl "test" >>=
  Synker.sync

let () = 
  Random.self_init ();
  Lwt_main.run @@ main ()
