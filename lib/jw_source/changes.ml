
open Base
open Lwt.Infix
open Sexplib.Std

module type VS = Sync.Variable_store


module Record = struct
  type t =
    { timestamp : int
    ; expires : int option
    ; passthrough : bool
    ; } [@@deriving sexp]

  let make ?expires ~passthrough () =
    { timestamp = Unix.time () |> Int.of_float; expires; passthrough }

  let to_string t =
    t |> sexp_of_t |> Sexplib.Sexp.to_string

  let of_string s =
    s |> Sexplib.Sexp.of_string |> t_of_sexp
end


let make_record_key media_id = "video-changed-" ^ media_id


let get_record (module Store : VS) media_id =
  let key = make_record_key media_id in

  match%lwt Store.get_opt key with
  | None   -> Lwt.return @@ Record.make ~passthrough:false ()
  | Some v -> Lwt.return @@ Record.of_string v


let set_record (module Store : VS) media_id changes =
  let key = make_record_key media_id in
  let value = changes |> Record.to_string in

  Store.set key value


let clear_record (module Store : VS) media_id =
  let key = make_record_key media_id in

  Store.delete key


let get_all_records (module Store : VS) ?except:(blacklist=[]) ?(min_age=0) () =
  let now = Unix.time () |> Int.of_float in

  let prefix = make_record_key "" in
  let pattern = make_record_key "%" in

  Store.get_like pattern

  >|= List.map ~f:begin fun (key, changes) ->
    let media_id =
      String.substr_replace_first key ~pattern:prefix ~with_:"" in
    (media_id, changes)
  end

  >|= List.filter ~f:begin fun (media_id, _) ->
    Option.is_none @@ List.find blacklist ~f:(String.equal media_id)
  end

  >|= List.map ~f:(fun (m, c) -> m, Record.of_string c)

  >|= List.filter ~f:begin fun ((_, { timestamp; _ }) : string * Record.t) ->
      (now - timestamp) > min_age
  end