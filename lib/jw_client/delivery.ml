
open Lwt.Infix
open Printf
open Util

module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix

let lplf = Lwt_io.printlf

type v2_media_body = V2_media_body_t.t

let api_prefix_url = "https://cdn.jwplayer.com/v2"

let get path ?(params=[]) () =
  let query = Uri.encoded_of_query params in
  let uri = [ api_prefix_url; path; "?"; query; ] |> String.concat ""
            |> Uri.of_string in

  try%lwt Clu.Client.get uri with
  | Unix.Unix_error(Unix.ETIMEDOUT, _, _) ->
    raise @@ Exn.Timeout ("GET", (Uri.to_string uri))
  | exn ->
    raise @@ Exn.Request_failure ("GET", (Uri.to_string uri), exn)

let get_media media_id ?params () =
  let path = "/media/" ^ media_id in
  get path ?params () >>= fun (resp, body) ->
  
  let status = resp |> C.Response.status in
  let code   = C.Code.code_of_status status in

  match C.Code.is_success code, status with
  | false, `Not_found -> Lwt.return None
  | false,          _ ->
    Exn.unexpected_response_status ~path ?params ~resp ~body () >>= raise
  | true,           _ -> 
    Clwt.Body.to_string body >|= fun body ->
    Some (V2_media_body_j.t_of_string body)

