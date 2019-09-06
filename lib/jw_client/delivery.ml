

open Exn
open Base
open Lwt.Infix
open Common.Lwt_result.Just_let_syntax


module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix
module Lwt_result = Common.Lwt_result


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
    Clu.Client.get uri >>= Lwt_result.return

  with
  | Unix.Unix_error(Unix.ETIMEDOUT, _, _) ->
    Lwt_result.fail @@ Timeout ("GET", (Uri.to_string uri))

  | exn ->
    Lwt_result.fail @@ unknown_request_failure "GET" uri exn


let get_media media_id ?params () =
  let path = "/media/" ^ media_id in
  
  let%bind resp, body = get path ?params () in
  
  let status = resp |> C.Response.status in
  let code   = C.Code.code_of_status status in

  match C.Code.is_success code, status with
  | false, `Not_found ->
    Lwt_result.return None

  | false,          _ ->
    Lwt_result.fail_lwt @@
      unexpected_response_status ~path ?params ~resp ~body ()

  | true,           _ -> 
    let%lwt body = Clwt.Body.to_string body in
    Lwt_result.return @@ Some (V2_media_body_j.t_of_string body)

