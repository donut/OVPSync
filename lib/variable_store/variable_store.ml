
open Lwt.Infix


module type Config = sig
  val namespace : string
end


module Make (DB : Caqti_lwt.CONNECTION) (Conf : Config) = struct

  let set key value =
    DB.exec Insert.or_update_var (Conf.namespace, key, value)
    >>= Caqti_lwt.or_fail 

  let get key ?default () =
    DB.find_opt Select.var_by_key (Conf.namespace, key)
    >>= Caqti_lwt.or_fail >>= function
    | Some v -> Lwt.return v
    |  None ->
      begin match default with
      | Some v -> Lwt.return v
      | None -> raise Not_found
      end

  let get_opt key =
    DB.find_opt Select.var_by_key (Conf.namespace, key)
    >>= Caqti_lwt.or_fail

  let get_like key_pattern =
    DB.collect_list Select.vars_with_key_like (Conf.namespace, key_pattern)
    >>= Caqti_lwt.or_fail

  let delete key =
    DB.exec Delete.var_by_key (Conf.namespace, key)
    >>= Caqti_lwt.or_fail

end