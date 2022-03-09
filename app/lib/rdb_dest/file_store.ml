
open Lwt.Infix


let minimum_required_space = 5 * 1024 * 1024 * 1024
(** Minimum space that should be avialable when choosing a location. *)


let spf = Printf.sprintf


let check_free_space path =
  (* Set block size to 1 so that we get the value in bytes instead of 
     kibibytes. *)
  let command = spf "df --sync --output=avail --block-size=1 %s" path in
  let ic = Unix.open_process_in command in
  let all_input = ref [] in

  begin 
    try 
      while true do
        all_input := input_line ic :: !all_input
      done
    with End_of_file ->
      close_in ic
  end;

  match !all_input with
  | [] | [_] -> 
    Error (`Error, None, "No output from `df` for path " ^ path)

  | available :: _ -> begin
    match available |> int_of_string_opt with
    | None ->
      let all_input = !all_input |> String.concat "\n---> " in
      Error 
        ( `Error
        , None
        , spf 
          "Couldn't convert '%s' to int as available space for [%s]. Full output:\n---> %s\n"
          available path all_input )

    | Some available ->
        Ok available
  end


type pick = 
  { path : string option
  ; available_space : int
  ; required_space : int 
  ; checked : (string * int) list }


let pick ?required_space stores =
  let required_space = 
    required_space |> BatOption.default minimum_required_space in

  let rec check ?(checked=[]) ~required_space stores =
    match stores with
    | [] ->
      Ok { path=None; available_space=0; required_space; checked }

    | hd :: tl ->
      match hd |> check_free_space with
      | Error _ as err ->
        err

      | Ok available_space when available_space > required_space ->
        Ok { path=(Some hd); available_space; required_space; checked }

      | Ok available_space ->
        let checked = (hd, available_space) :: checked in
        check ~checked ~required_space tl
  in

  check ~required_space stores


let local_scheme = "local"


let make_local_uri rel_path filename =
  let rel_path = 
    rel_path |> File.trim_slashes |> String.split_on_char '/'
    |> List.map Uri.pct_encode |> String.concat "/" in
  let filename = Uri.pct_encode filename in
  spf "%s:///%s/%s" local_scheme rel_path filename


let is_local_uri uri =
  Uri.scheme uri |> (=) (Some local_scheme)


let abs_path_of_local_uri store uri =
  let rel_path = 
    Uri.path uri |> File.trim_slashes
    |> String.split_on_char '/' |> List.map Uri.pct_decode
    |> String.concat "/" in
  spf "%s/%s" store rel_path


let rec store_of_local_uri stores uri =
  match stores with
  | [] -> 
    Lwt.return None

  | store :: stores ->
    if not (uri |> is_local_uri) then Lwt.return None
    else

    uri 
    |> abs_path_of_local_uri store
    |> Lwt_unix.file_exists
    >>= function 
    | false -> store_of_local_uri stores uri
    | true -> Lwt.return (Some store)


let unlink_uri_if_found_on_stores stores uri =
  match%lwt uri |> store_of_local_uri stores with
  | None -> 
    Lwt.return ()

  | Some store ->
    uri
    |> abs_path_of_local_uri store
    |> File.unlink_if_exists


module File = struct
  type t =
    { store : string
    ; uri : Uri.t
    ; size : int }

  let of_local_uri stores uri = 
    match%lwt uri |> store_of_local_uri stores with
    | None -> 
      Lwt.return None

    | Some store ->
      let abs_path = uri |> abs_path_of_local_uri store in
      let%lwt stats = abs_path |> Lwt_unix.stat in
      Lwt.return (Some { store; uri; size = stats.st_size })
end