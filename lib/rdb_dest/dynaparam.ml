
(* A method for dynamically setting the parameters at run time.
 * @see https://github.com/paurkedal/ocaml-caqti/issues/15
 * @see http://paurkedal.github.io/ocaml-caqti/caqti/Caqti_request/index.html#how-to-dynamically-assemble-queries-and-parameters
 *)

type t = Pack : 'a Caqti_type.t * 'a * string list -> t
let empty = Pack (Caqti_type.unit, (), [])
let add t x p (Pack (t', x', pl)) =
  Pack (Caqti_type.tup2 t' t, (x', x), p :: pl)