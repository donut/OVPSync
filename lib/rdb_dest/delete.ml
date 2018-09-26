
module type DBC = Caqti_lwt.CONNECTION

module Q = struct 

  module Creq = Caqti_request
  open Caqti_type

  let source = Creq.exec
    int
    "DELETE FROM source WHERE id = ? LIMIT 1"

  let source_field = Creq.exec
    (tup2 int string)
    "DELETE FROM source_field WHERE source_id = ? AND name = ? LIMIT 1"

end

let source pl id = Util.exec pl Q.source id

let x_fields_not_named pl x x_id names =
  let x_name = match x with `Source -> "source" | `Video -> "video" in
  let module D = Dynaparam in
  let (D.Pack (typ, vals, placeholders)) = List.fold_left 
    (fun pack name -> D.add Caqti_type.string name "?" pack)
    D.empty names in
  let typ = Caqti_type.(tup2 int typ) in
  let vals = (x_id, vals) in
  let placeholders = String.concat ", " placeholders in
  let sql = Printf.sprintf
    "DELETE FROM %s_field WHERE %s_id = ? AND name NOT IN (%s)"
    x_name x_name placeholders in
  let query = Caqti_request.exec typ sql in
  Util.exec pl query vals

let source_fields_not_named pl src_id names =
  x_fields_not_named pl `Source src_id names

let source_field pl source_id name =
  Util.exec pl Q.source_field (source_id, name)

let video_fields_not_named pl vid_id names =
  x_fields_not_named pl `Video vid_id names