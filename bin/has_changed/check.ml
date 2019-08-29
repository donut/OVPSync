
open Lib.Infix

module Blist = BatList

module type Config = sig
  val item_log_id : string
end


module type Check = sig
  (* val int_field : string -> old:int -> new':int -> bool Lwt.t

  val uri_field
     : string -> old:(Uri.t option) -> new':(Uri.t option)
    -> bool Lwt.t

  val string_field : string -> old:string -> new':string -> bool Lwt.t *)
end


module Make (Log : Logger.Sig)(Conf : Config) : Check = struct

  let log_id = Conf.item_log_id


  let field ~name ~has_changed ~to_log a b =
    if has_changed a b then
      let%lwt () = Log.debugf "[%s] %s has changed." log_id name in
      let%lwt () = Log.tracef "[%s] %s." log_id (to_log a b) in
      Lwt.return true

    else Lwt.return false


  let int_field name ~old ~new' =
    let to_log old new' = 
      Printf.sprintf "%s changed from [%d] to [%d]." name old new' in
    field ~name ~has_changed:(<>) ~to_log old new' 


  let uri_field name ~old ~new' =
    let has_changed old new' =
      (* URIs are not compared if the old URIs are local. If they're not local, 
        it implies that either no URI was previously provided or there was a
        failure saving the file. *)
      let old_is_local = 
        old >|? (Uri.scheme %> (=) (Some Rdb_dest.local_scheme)) =?: false
      in

       not old_is_local 
       (* If file was not saved previously, unless URI was always [None],
          this needs another attempt at syncing. *)
       && not (old = None && new' = None)
    in

    let to_log old new' = 
      let str a = a >|? Uri.to_string =?: "`None`" in
      Printf.sprintf 
        "%s changed from [%s] to [%s]." name (str old) (str new')
    in
      
    field ~name ~has_changed ~to_log old new'


  let string_field name ~old ~new' =
    let to_log old new' = 
      Printf.sprintf "%s changed from [%s] to [%s]" name old new' in
    field ~name ~has_changed:(<>) ~to_log old new' 


  let list_field name ~to_log a b =
    let rec has_changed a b =
      if List.compare_lengths a b <> 0 then true
      else

      match a with
      | [] -> not @@ Blist.is_empty b
      | hd :: tl ->
        let filter = List.filter ((<>) hd) in
        has_changed (filter tl) (filter b)
    in

    field ~name ~has_changed ~to_log a b


  let string_list_field name ~old ~new' =
    let to_log old new' =
      let str = String.concat "; " in
      Printf.sprintf "%s changed from [%s] to [%s]"
        name (str old) (str new')
    in

    list_field name ~to_log old new'


  let string_pair_list_field name ~old ~new' =
    let to_log old new' =
      let str = 
        List.map (fun (k, v) -> k ^ ": " ^ v)
        %> String.concat "; "
      in
      Printf.sprintf "%s changed from [%s] to [%s]"
        name (str old) (str new')
    in

    list_field name ~to_log old new'

  
  let source_field_has_changed a b = 
      try Rdb_dest.Source.has_changed a b with | _ -> true

  
  let source_list_field_to_log name old new' =
    let string_of_source src =
      let string_of_custom = 
        List.map (fun (key, value) -> key ^ ": " ^ value)
        %> String.concat "; "
      in

      let open Rdb_dest.Source in
      Printf.sprintf 
        "  { name = %s; media_id = %s; added = %s\n  ; custom = [%s] }"
        (name src) 
        (media_id src) 
        (added src |> BatInt.to_string) 
        (custom src |> string_of_custom)
    in

    let string_of_list l =
      l |> List.map string_of_source |> String.concat "\n" in

    let old = string_of_list old in
    let new' = string_of_list new' in
    Printf.sprintf "%s changed\n--> from:\n%s\n--> to:\n%s" name old new'


  let source_field name ~old ~new' =
    let to_log old new' = source_list_field_to_log name [old] [new'] in
    field ~name ~has_changed:source_field_has_changed ~to_log old new'


  let source_list_field name ~old ~new' =
    let has_changed la lb =
      if List.compare_lengths old new' <> 0 then true
      else

      la |> List.exists begin fun a ->
        match List.find_opt (Rdb_dest.Source.are_same a) lb with
        | None -> true
        | Some b -> source_field_has_changed a b
      end
    in

    let to_log = source_list_field_to_log name in

    field ~name ~has_changed ~to_log old new'

  let optional name check ~old ~new' =
    match old, new' with
    | Some old, Some new' -> check name ~old ~new'

    | _ ->
      let has_changed a b =
        match a, b with
        | None, None -> false
        | _ -> true
      in

      let to_log a b =
        let str x = x >|? (fun _ -> "Some") =?: "None" in
        Printf.sprintf "%s changed from [`%s`] to [`%s`]" name (str a) (str b)
      in

      field ~name ~has_changed ~to_log old new'

end