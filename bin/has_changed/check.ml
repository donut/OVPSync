
open Lib.Infix.Function
open Lib.Infix.Option

module Blist = BatList


type ('value, 'result) runner 
   = has_changed:('value -> 'value -> bool)
  -> to_log:('value -> 'value -> string)
  -> 'value
  -> 'value
  -> 'result


let test__run ~has_changed ~to_log:_ a b = has_changed a b


let int_field run name ~old ~new' =
  let to_log old new' = 
    Printf.sprintf "%s changed from [%d] to [%d]" name old new' in
  run ~has_changed:(<>) ~to_log old new' 


let%test "int_field no change" =
  int_field test__run "" ~old:0 ~new':0 = false

let%test "int_field has change" =
  int_field test__run "" ~old:0 ~new':1 = true


let uri_field run name ~old ~new' =
  let has_changed a b = not @@ Uri.equal a b in

  let to_log old new' =
    Printf.sprintf "%s changed from [%s] to [%s]"
      name (Uri.to_string old) (Uri.to_string new')
  in

  run ~has_changed ~to_log old new'


let%test "uri_field no change" =
  let old = Uri.of_string 
    "https://www.example.com:234/example/dir?yes=no&no=yes&#hashtag-purple" in
  let new' = Uri.of_string 
    "https://www.example.com:234/example/dir?yes=no&no=yes&#hashtag-purple" in
  uri_field test__run "" ~old ~new' = false

let%test "uri_field has change" =
  let old = Uri.of_string 
    "https://www.example.com:234/example/dir?no=yes&yes=no&#hashtag-purple" in
  let new' = Uri.of_string 
    "https://www.example.com:234/example/dir?no=yes&yes=no&#hashtag-green" in
  uri_field test__run "" ~old ~new' = true


let saveable_uri_field run name ~old ~new' =
  let has_changed old new' =
    (* URIs are not compared if the old URI is local. If it's not local, 
      it implies that either no URI was previously provided or there was a
      failure saving the file. *)
    let old_is_local = old >|? Rdb_dest.is_local_uri =?: false in

      not old_is_local 
      (* If file was not saved previously, unless URI was always [None],
        this needs another attempt at syncing. *)
      && not (old = None && new' = None)
  in

  let to_log _old _new' = 
    Printf.sprintf "%s has not been downloaded previously" name in
    
  run ~has_changed ~to_log old new'


let%test "saveable_uri_field with local old URI" =
  let old = Some (Uri.of_string "local:///2016/08/19/example.mov") in
  let new' = None in
  saveable_uri_field test__run "" ~old ~new' = false

let%test "saveable_uri_field without old or new URIs" =
  let old = None in
  let new' = None in
  saveable_uri_field test__run "" ~old ~new' = false

let%test "saveable_uri_field with remote old URI but no new URI" =
  let old = Some (Uri.of_string "https://isitchristmas.com") in
  let new' = None in
  saveable_uri_field test__run "" ~old ~new' = true

let%test "saveable_uri_field with only new URI" =
  let old = None in
  let new' = Some (Uri.of_string "https://isitchristmas.com") in
  saveable_uri_field test__run "" ~old ~new' = true

let%test "saveable_uri_field with same old & new URIs" =
  let old = Some (Uri.of_string "https://isitchristmas.com") in
  let new' = Some (Uri.of_string "https://isitchristmas.com") in
  saveable_uri_field test__run "" ~old ~new' = true

let%test "saveable_uri_field with remote but different old & new URIs" =
  let old = Some (Uri.of_string "https://isitchristmas.com") in
  let new' = Some (Uri.of_string "https://www.smbc-comics.com") in
  saveable_uri_field test__run "" ~old ~new' = true


let string_field run name ~old ~new' =
  let to_log old new' = 
    Printf.sprintf "%s changed from [%s] to [%s]" name old new' in
  run ~has_changed:(<>) ~to_log old new' 


let%test "string_field no change" =
  string_field test__run "" ~old:"same" ~new':"same" = false

let%test "string_field has change" =
  string_field test__run "" ~old:"not" ~new':"same" = true


let list_field run ~to_log a b =
  let rec has_changed a b =
    if List.compare_lengths a b <> 0 then true
    else

    match a with
    | [] -> not @@ Blist.is_empty b
    | hd :: tl ->
      let filter = List.filter ((<>) hd) in
      has_changed (filter tl) (filter b)
  in

  run ~has_changed ~to_log a b


let string_list_field run name ~old ~new' =
  let to_log old new' =
    let str = String.concat "; " in
    Printf.sprintf "%s changed from [%s] to [%s]"
      name (str old) (str new')
  in

  list_field run ~to_log old new'


let%test "string_list_field no change" =
  not @@ string_list_field test__run "" 
    ~old:["are"; "same"] 
    ~new':["same"; "are"]

let%test "string_list_field has change" =
  string_list_field test__run "" 
    ~old:["are"; "not"] 
    ~new':["the"; "same"]


let string_pair_list_field run name ~old ~new' =
  let to_log old new' =
    let str = 
      List.map (fun (k, v) -> k ^ ": " ^ v)
      %> String.concat "; "
    in
    Printf.sprintf "%s changed from [%s] to [%s]"
      name (str old) (str new')
  in

  list_field run ~to_log old new'


let%test "string_pair_list_field no change" =
  not @@ string_pair_list_field test__run "" 
    ~old:["are", "totally"; "the", "same"] 
    ~new':["the", "same"; "are", "totally"]

let%test "string_pair_list_field has change" =
  string_pair_list_field test__run "" 
    ~old:["are", "totally"; "not", "same"] 
    ~new':["the", "same"; "are", "not"]


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


let source_field run name ~old ~new' =
  let to_log old new' = source_list_field_to_log name [old] [new'] in
  run ~has_changed:source_field_has_changed ~to_log old new'


let source_list_field run name ~old ~new' =
  let has_changed old new' =
    (* [old], having been previously saved, may have sources that [new'] knows
       nothing about. So we only care about sources in [new'] that have are
       different than their [old] counterparts are don't exist in [old]. *)
    new' |> List.exists begin fun n ->
      match List.find_opt (Rdb_dest.Source.are_same n) old with
      | None -> true
      | Some o -> source_field_has_changed o n
    end
  in

  let to_log = source_list_field_to_log name in

  run ~has_changed ~to_log old new'


let%test_module "source_field" = (module struct
  open Rdb_dest.Source

  let base = 
    { id = None
    ; name = "jw-12345"
    ; media_id = "1234"
    ; video_id = None
    ; added = 1234
    ; modified = 1234
    ; custom = ["one", "1"; "two", "2"] }

  let diff_id = { base with id = Some 1324 } 
  let diff_name = { base with name = "ooyala" }
  let diff_media_id = { base with media_id = "5678" }
  let diff_video_id = { base with video_id = Some 5678 }
  let diff_added = { base with added = 5678 }
  let diff_modified = { base with modified = 5678 }
  let diff_custom = { base with custom = ["one", "1"; "three", "3"] }

  let%test "source_field no change" =
    not @@ source_field test__run "" ~old:base ~new':base

  let%test "source_field different id" =
    not @@ source_field test__run "" ~old:base ~new':diff_id

  let%test "source_field different name" =
    source_field test__run "" ~old:base ~new':diff_name

  let%test "source_field different media_id" =
    source_field test__run "" ~old:base ~new':diff_media_id

  let%test "source_field different video_id" =
    not @@ source_field test__run "" ~old:base ~new':diff_video_id

  let%test "source_field different added" =
    source_field test__run "" ~old:base ~new':diff_added

  let%test "source_field different modified" =
    not @@ source_field test__run "" ~old:base ~new':diff_modified

  let%test "source_field different custom" =
    source_field test__run "" ~old:base ~new':diff_custom

  let jw = base
  let ooyala = 
    { jw with
      name = "ooyala"
    ; media_id = "BiYmFmNTE6Ael76VbXQjh72oxtSkyM0_" }
  let ovp_migrate = 
    { base with
      name = "ovp_migrate"
    ; media_id = "a96bb757be39263a17170100" }
    
  let%test "source_list_field old and new are same" =
    let old = [jw; ooyala; ovp_migrate] in
    let new' = old in
    not @@ source_list_field test__run "" ~old ~new'

  let%test "source_list_field fewer but no different sources in new" =
    let old = [jw; ooyala; ovp_migrate] in
    let new' = [jw] in
    not @@ source_list_field test__run "" ~old ~new'

  let%test "source_list_field changed source in new" =
    let old = [jw; ooyala; ovp_migrate] in
    let jw' = { jw with added = 12345 } in
    let new' = [jw'] in
    source_list_field test__run "" ~old ~new'

  let%test "source_list_field additional source in new, but not in old" =
    let old = [jw; ooyala; ovp_migrate] in
    let jw' = { jw with media_id = "goomba" } in
    let new' = [jw; jw'] in
    source_list_field test__run "" ~old ~new'

  let%test "source_list_field only new source in new" =
    let old = [jw; ooyala; ovp_migrate] in
    let jw' = { jw with media_id = "goomba" } in
    let new' = [jw'] in
    source_list_field test__run "" ~old ~new'
end)


let optional
  (run : ('value option, 'result) runner)
  (check : string -> old:'value -> new':'value -> 'result)
  name
  ~(old : 'value option)
  ~(new' : 'value option)
=
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

    run ~has_changed ~to_log old new'



module type Config = sig
  type result
  val runner : ('value, result) runner
end


module Make = functor (Conf : Config) -> struct
  type result = Conf.result

  let run = Conf.runner

  let int_field = int_field run

  let string_field = string_field run
  let string_list_field = string_list_field run
  let string_pair_list_field = string_pair_list_field run

  let source_field = source_field run
  let source_list_field = source_list_field run

  let uri_field = uri_field run
  let saveable_uri_field = saveable_uri_field run

  let optional check name ~old ~new' = optional run check name ~old ~new'
end