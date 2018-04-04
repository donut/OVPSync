
open Lwt.Infix

module Creq = Caqti_request
open Caqti_type

let var_by_key = Creq.find_opt
  (tup2 string string) string
  "SELECT value FROM variable WHERE namespace = ? AND `key` = ?"

let vars_with_key_like = Creq.collect
  (tup2 string string) (tup2 string string)
  "SELECT `key`, value FROM variable WHERE namespace = ? AND `key` LIKE ?"
