
module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix

open Lwt.Infix

let spf = Printf.sprintf
let lplf fmt = Printf.ksprintf (Lwt_io.printl) fmt
let plf fmt = Printf.ksprintf (print_endline) fmt

exception File_error of string * string * string
exception Request_failure of string * string * exn
exception Timeout of string * string
exception Unexpected_response_status of string * string * string

let unexpected_response_status_exn
  ?(meth="GET") ?(params=[]) ~path ~resp ~body ()
=
  let request =
    let query = Uri.encoded_of_query params in
    [ path; "?"; query; ] |> String.concat ""
  in
  let%lwt resp_str = 
    let status = Cohttp.Response.status resp |> Cohttp.Code.string_of_status in
    let headers = Cohttp.Response.headers resp |> Cohttp.Header.to_string in
    let%lwt body = Cohttp_lwt.Body.to_string body in
    Printf.sprintf
      "### Status: %s ###\n \
       ### Headers ###\n%s\n\n \
       ### Body ###\n%s\n### END Body ###\n"
      status headers body 
    |> Lwt.return
  in
  Lwt.return @@ Unexpected_response_status (meth, request, resp_str)

let max_name_length = 255
(* A safe value for most operating systems.
   @see https://serverfault.com/a/9548/54523 *)

let md5 path = Digest.file path |> Digest.to_hex

let sanitize name =
  let ptrn = Re.Perl.compile_pat "[\\/:]" in
  Re.replace_string ~all:true ptrn ~by:"-" name

let dir_of_timestamp ts =
  let tm = Unix.gmtime (ts |> float_of_int) in
  let { tm_year; tm_mon; tm_mday } : Unix.tm = tm in
  spf "%04d/%02d/%02d" (tm_year + 1900) (tm_mon + 1) tm_mday

let trim_slashes p =
  let ptrn = Re.Perl.compile_pat "^/+|/+$" in
  Re.replace_string ~all:true ptrn ~by:"" p

let check_dir path =
  let%lwt { st_kind; st_perm } = Lwt_unix.stat path in
  if not (st_kind = Unix.S_DIR) then
    (* @todo test with symbolic link to directory. Will [st_kind] be S_LNK
              or still S_DIR. *)
    raise @@ File_error (path, "is not a directory", "");
  if st_perm < 0o700 then 
    raise @@ File_error (path, "permissions less than 0700", "");
  Lwt.return () 


(** [prepare_dir ~prefix path] Creates all intermediate directories from
    [prefix] to [path], including [prefix]. *)
let rec prepare_dir ~prefix path =
  let path = trim_slashes path in
  begin match%lwt Lwt_unix.file_exists prefix with 
  | false ->
    begin try%lwt Lwt_unix.mkdir prefix 0o775 with
    | Unix.Unix_error(Unix.EEXIST, "mkdir", _) ->
      (* Because several saves can be run in parallel, between checking if the
         the dir exists and trying to create it, another LWT could have created
         it. *)
      check_dir prefix
    | exn ->
      let exn = Printexc.to_string exn in
      raise @@ File_error (prefix, "failed creating dir", exn)
    end
  | true -> check_dir prefix
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
    | [| _; e |] ->
      let nums = Re.Perl.compile_pat "^\\d+$" in
      begin match Re.exec_opt nums e with
      (* An extenion made only of numbers is probably not a real extension. *)
      | Some _ -> None
      | None -> Some e
      end
    | g ->
      None

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
      (* [-3] for the --- to show that it was shortened
         [-1] for the . separating the basename and extension *)
      let sub = String.sub base 0 (max_name_length - 4) in
      spf "%s---.%s" sub ext

  (** [get_uri uri] The same as [Cohttp_lwt_unix.Client.get] but follows
      redirects. *)
  let rec get_uri ?(redirects=30) uri =
    let%lwt (resp, body) = try%lwt Clu.Client.get uri with
      | Unix.Unix_error(Unix.ETIMEDOUT, _, _) ->
        raise @@ Timeout ("GET", (Uri.to_string uri))
      | exn ->
        raise @@ Request_failure ("GET", (Uri.to_string uri), exn)
    in

    (* Cut off redirect loop. *)
    if redirects = 0
    then Lwt.return (resp, body)
    else

    match C.Response.status resp with
    | `Found | `Moved_permanently | `See_other | `Temporary_redirect ->
      begin match C.Header.get (C.Response.headers resp) "location" with
      | None -> Lwt.return (resp, body)
      | Some l -> get_uri ~redirects:(redirects - 1) (Uri.of_string l)
      end
    | _ -> Lwt.return (resp, body)

  let save src ~to_ =
    let perms = Lwt_unix.([ O_WRONLY; O_CREAT; O_TRUNC ]) in
    let%lwt file = try%lwt Lwt_unix.openfile to_ perms 0o664 with
    | exn ->
      let exn = Printexc.to_string exn in
      raise @@ File_error (to_, "Failed opening/creating file", exn)
    in
    let fch = Lwt_io.of_fd ~mode:Output file in
    
    let%lwt (resp, body) = get_uri src in

    if C.Response.status resp <> `OK then
      Lwt_io.close fch >>= fun () ->
      Lwt_unix.unlink to_ >>= fun () ->
      unexpected_response_status_exn ~path:(Uri.to_string src) ~resp ~body ()
      >>= raise
    else

    Clwt.Body.to_stream body |> Lwt_stream.iter_s (Lwt_io.write fch)
      >>= fun () ->

    Lwt_io.close fch

  let unlink_if_exists path =
    if%lwt Lwt_unix.file_exists path
    then Lwt_unix.unlink path
    else Lwt.return ()