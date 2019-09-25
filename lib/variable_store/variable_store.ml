
open Base
open Lwt.Infix

module Cpool = Caqti_lwt.Pool

module type Config = sig
  val db_pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t
  val namespace : string
end

module Make (Conf : Config) = struct
  open Conf

  let set key value =
    db_pool |> Cpool.use (fun (module DB : Caqti_lwt.CONNECTION) ->
      DB.exec Insert.or_update_var (namespace, key, value))
    >>= Caqti_lwt.or_fail 

  let get key ?default () =
    db_pool |> Cpool.use (fun (module DB : Caqti_lwt.CONNECTION) ->
      DB.find_opt Select.var_by_key (Conf.namespace, key))
    >>= Caqti_lwt.or_fail >>= function
    | Some v -> Lwt.return v
    | None ->
      begin match default with
      | Some v -> Lwt.return v
      | None -> raise @@ Not_found_s (Sexplib0.Sexp_conv.sexp_of_string key)
      end

  let get_opt key =
    db_pool |> Cpool.use (fun (module DB : Caqti_lwt.CONNECTION) ->
      DB.find_opt Select.var_by_key (Conf.namespace, key))
    >>= Caqti_lwt.or_fail

  let get_like key_pattern =
    db_pool |> Cpool.use (fun (module DB : Caqti_lwt.CONNECTION) ->
      DB.collect_list Select.vars_with_key_like (Conf.namespace, key_pattern))
    >>= Caqti_lwt.or_fail

  let delete key =
    db_pool |> Cpool.use (fun (module DB : Caqti_lwt.CONNECTION) ->
      DB.exec Delete.var_by_key (Conf.namespace, key))
    >>= Caqti_lwt.or_fail

end