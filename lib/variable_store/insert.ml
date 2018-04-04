
open Lwt.Infix


module Creq = Caqti_request
open Caqti_type

let or_update_var = Creq.exec 
  (tup3 string string string)
  "INSERT INTO variable (namespace, `key`, value) VALUES (?, ?, ?) \
   ON DUPLICATE KEY UPDATE value = value"
