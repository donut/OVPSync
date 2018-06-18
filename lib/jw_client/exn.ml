
open Lwt.Infix

exception Request_failure of string * string * string
exception Timeout of string * string
exception Unexpected_response_status of string * string * string

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