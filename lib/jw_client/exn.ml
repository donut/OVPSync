
exception Request_failure of string * string * string
exception Temprorary_error of string * string * string
exception Timeout of string * string
exception Unexpected_response_status of string * string * string

let unknown_request_failure meth uri exn =
  (* Handles this error:
     > Failure
        "TLS to non-TCP currently unsupported: host=cdn.jwplayer.com endp=(Unknown \"name resolution failed\")"
    Had this happen intermittently on a Hackintosh, and suspect it has
    something to do with that system. 
    @see https://github.com/mirage/ocaml-conduit/issues/266
  *)
  let exn = Printexc.to_string exn in
  let ptrn = Re.Perl.compile_pat "name resolution failed" in
  match Re.exec_opt ptrn exn with
  | None -> Request_failure (meth, (Uri.to_string uri), exn)
  | Some _ -> Temprorary_error (meth, (Uri.to_string uri), exn)

let unexpected_response_status ?(meth="GET") ?(params=[]) ~path ~resp ~body () =
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