
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