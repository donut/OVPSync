
open Lwt.Infix

module type DBC = Caqti_lwt.CONNECTION

module Q = struct 

  module Creq = Caqti_request
  open Caqti_type

  let source_field = Creq.exec
    (tup2 int string)
    "DELETE FROM source_field WHERE source_id = ? AND name = ? LIMIT 1"

end

let source_field (module DB : DBC) source_id name =
  DB.exec Q.source_field (source_id, name) >>= Caqti_lwt.or_fail