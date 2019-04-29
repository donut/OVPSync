
module type DBC = Caqti_lwt.CONNECTION
module Bopt = BatOption

open Lwt.Infix

exception Row_not_found of string
exception Last_insert_ID_is_zero

module Q = struct
  module Creq = Caqti_request
  open Caqti_type

  let last_insert_id = Creq.find unit int "SELECT LAST_INSERT_ID()"
end

let last_insert_id (module DB : DBC) =
  match%lwt DB.find Q.last_insert_id () >>= Caqti_lwt.or_fail with 
  |  0 -> raise Last_insert_ID_is_zero
  | id -> Lwt.return id

let ptime_of_int i =
  i |> float_of_int |> Ptime.of_float_s |> Bopt.get
let int_of_ptime p =
  p |> Ptime.to_float_s |> int_of_float


let use_pool p f =
  let%lwt result = p |> Caqti_lwt.Pool.use begin fun dbc ->
    match%lwt f dbc with
    | exception exn -> Lwt.return @@ Ok (Error exn)
    | x -> Lwt.return @@ Ok (Ok x)
  end >>= Caqti_lwt.or_fail in

  match result with
  | Error exn -> raise exn
  | Ok x -> Lwt.return x


let run p_or_c f =
  match p_or_c with
  | `Connection c -> f c
  | `Pool p -> use_pool p f


let exec porc query values = run porc (fun (module DB : DBC) ->  
  DB.exec query values >>= Caqti_lwt.or_fail)


let insert proc query values = run proc begin fun dbc -> 
  let c = `Connection dbc in
  exec c query values >>= fun () ->
  run c last_insert_id 
end

let collect_list porc query values = run porc (fun (module DB : DBC) -> 
  DB.collect_list query values >>= Caqti_lwt.or_fail)

let find_opt porc query values = run porc (fun (module DB : DBC) ->
  DB.find_opt query values >>= Caqti_lwt.or_fail)

let items_are_same la lb =
  let have_same () =
    not @@ List.exists (fun x -> not @@ List.exists ((=) x) lb) la in
  List.compare_lengths la lb = 0 || have_same ()