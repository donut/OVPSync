
open Lwt.Infix

exception Unexpected_response_status of string * string * string
exception Request_failure of string * string * exn
exception Timeout of string * string

let unexpected_response_status resp body =
  let status = resp |> Cohttp.Response.status |> Cohttp.Code.string_of_status in
  let headers = resp |> Cohttp.Response.headers |> Cohttp.Header.to_string in
  Cohttp_lwt.Body.to_string body >>= fun body' ->
  Lwt.return @@ Unexpected_response_status (status, headers, body')