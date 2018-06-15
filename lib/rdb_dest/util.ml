
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

let use_pool p f = Caqti_lwt.Pool.use f p >>= Caqti_lwt.or_fail

let use_pool' p f =
  p |> Caqti_lwt.Pool.use begin fun (module DB : DBC) ->
    try%lwt f (module DB : DBC) >|= fun x -> Ok (Ok x) with
    | exn -> Lwt.return @@ Ok (Error exn)
  end >>= Caqti_lwt.or_fail >|= function
  | Error exn -> raise exn
  | Ok x -> x

let exec pool query values = 
  use_pool pool (fun (module DB : DBC) -> DB.exec query values)

let insert pool query values =
  use_pool' pool begin fun (module DB : DBC) ->
    DB.exec query values >>= Caqti_lwt.or_fail >>= fun () ->
    last_insert_id (module DB)
  end

let collect_list pool query values =
  use_pool pool (fun (module DB : DBC) -> DB.collect_list query values)

let find_opt pool query values =
  use_pool pool (fun (module DB : DBC) -> DB.find_opt query values)

let fields_have_changed old knew =
  let have_same () =
    not @@ List.exists (fun x -> not @@ List.exists ((=) x) knew) old in
  List.compare_lengths old knew <> 0
  || not (have_same ())