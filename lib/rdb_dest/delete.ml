
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


let source dbc id =
  Util.exec dbc Q.source id


let x_fields_not_named dbc x x_id names =
  if BatList.is_empty names then Lwt.return ()
  else

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
  let query = Caqti_request.exec ~oneshot:true typ sql in

  Util.exec dbc query vals


let source_fields_not_named dbc src_id names =
  x_fields_not_named dbc `Source src_id names

let source_field dbc source_id name =
  Util.exec dbc Q.source_field (source_id, name)

let video_fields_not_named dbc vid_id names =
  x_fields_not_named dbc `Video vid_id names