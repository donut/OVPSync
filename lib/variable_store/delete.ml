
open Lwt.Infix

module Creq = Caqti_request
open Caqti_type

let var_by_key = Creq.exec
  (tup2 string string)
  "DELETE FROM variable WHERE namespace = ? AND `key` = ? LIMIT 1"
