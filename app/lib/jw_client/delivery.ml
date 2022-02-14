

open Exn
open Base
open Lwt.Infix
open Lib.Result_lwt.Just_let_syntax


module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix
module Result_lwt = Lib.Result_lwt


type v2_media_body = V2_media_body_t.t

let api_prefix_url = "https://cdn.jwplayer.com/v2"


let get path ?(params=[]) () =
  let uri =
    let query = Uri.encoded_of_query params in
    [ api_prefix_url; path; "?"; query; ]
    |> String.concat
    |> Uri.of_string
  in

  try%lwt 
    Clu.Client.get uri >>= Result_lwt.return

  with
  | Unix.Unix_error(Unix.ETIMEDOUT, _, _) ->
    Result_lwt.fail @@ Timeout ("GET", (Uri.to_string uri))

  | exn ->
    Result_lwt.fail @@ unknown_request_failure "GET" uri exn


let get_media media_id ?params () =
  let path = "/media/" ^ media_id in
  
  let%bind resp, body = get path ?params () in
  
  let status = resp |> C.Response.status in
  let code   = C.Code.code_of_status status in

  match C.Code.is_success code, status with
  | false, `Not_found ->
    Result_lwt.return None

  | false,          _ ->
    Result_lwt.fail_lwt @@
      unexpected_response_status ~path ?params ~resp ~body ()

  | true,           _ -> 
    let%lwt body = Clwt.Body.to_string body in
    Result_lwt.try_return (fun () -> Some (V2_media_body_j.t_of_string body))

