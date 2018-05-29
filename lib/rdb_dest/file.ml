
module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix

open Lwt.Infix

let spf = Printf.sprintf

exception File_error of string * string * exn option
exception Unexpected_response_status of string * string * string

let max_name_length = 255
(* A safe value for most operating systems.
   @see https://serverfault.com/a/9548/54523 *)

let unexpected_response_status_exn resp body =
  let status = resp |> Cohttp.Response.status |> Cohttp.Code.string_of_status in
  let headers = resp |> Cohttp.Response.headers |> Cohttp.Header.to_string in
  Cohttp_lwt.Body.to_string body >>= fun body' ->
  Lwt.return @@ Unexpected_response_status (status, headers, body')

let dir_of_timestamp ts =
  let tm = Unix.gmtime (ts |> float_of_int) in
  let { tm_year; tm_mon; tm_mday } : Unix.tm = tm in
  spf "%04d/%02d/%02d" (tm_year + 1900) (tm_mon + 1) tm_mday

let trim_slashes p =
  let ptrn = Re.Perl.compile_pat "^/+|/+$" in
  Re.replace_string ~all:true ptrn ~by:"" p

(** [prepare_dir ~prefix path] Creates all intermediate directories from
    [prefix] to [path], including [prefix]. *)
let rec prepare_dir ~prefix path =
  let path = trim_slashes path in
  begin match%lwt Lwt_unix.file_exists prefix with 
  | false ->
    begin try%lwt Lwt_unix.mkdir prefix 0o775 with
    | exn -> raise @@ File_error (prefix, "failed creating dir", Some exn)
    end
  | true ->
    let%lwt { st_kind; st_perm } = Lwt_unix.stat prefix in
    if not (st_kind = Unix.S_DIR) then
      (* @todo test with symbolic link to directory. Will [st_kind] be S_LNK
               or still S_DIR. *)
      raise @@ File_error (prefix, "is not a directory", None);
    if st_perm < 0o700 then 
      raise @@ File_error (prefix, "permissions less than 0700", None);
    Lwt.return () 
  end >>= fun () ->
  match String.split_on_char '/' path with
  | [] | [""] -> Lwt.return prefix
  | hd :: tl ->
    let prefix = spf "%s/%s" prefix hd in
    prepare_dir ~prefix (tl |> String.concat "/")

let ext filename =
  let pattern = Re.Perl.compile_pat "\\.([\\w\\d]+)$" in
  match Re.exec_opt pattern filename with 
  | None -> None
  | Some g -> match Re.Group.all g with
    | [| e |] -> Some e
    | _ -> None

  let basename filename = 
    let b = filename |> String.split_on_char '/' |> BatList.last in
    match ext b with
    | None -> b
    | Some e ->
      let ptrn = Re.Perl.compile_pat @@ spf "\\.%s$" e in
      Re.replace_string ~all:false ptrn ~by:"" b

  let restrict_name_length base ext =
    let name = spf "%s.%s" base ext in
    let length = String.length name in
    if length <= max_name_length then 
      name
    else
      let sub = String.sub base 0 (max_name_length - 3) in
      spf "%s---.%s" sub ext

  let save src ~to_ =
    let perms = Lwt_unix.([ O_WRONLY; O_CREAT; O_EXCL ]) in
    let%lwt file = try%lwt Lwt_unix.openfile to_ perms 0o664 with
    | exn -> raise @@ File_error (to_, "Failed opening/creating file", Some exn)
    in
    let fch = Lwt_io.of_fd ~mode:Output file in
    
    let%lwt (resp, body) = Clu.Client.get src in

    if C.Response.status resp <> `OK then
      unexpected_response_status_exn resp body >>= raise
    else

    Clwt.Body.to_stream body |> Lwt_stream.iter_s (Lwt_io.write fch)
      >>= fun () ->

    Lwt_io.close fch