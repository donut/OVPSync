
open Lwt.Infix
open Printf
open Util

module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix

type v2_media_body = V2_media_body_t.t

let api_prefix_url = "https://cdn.jwplayer.com/v2"


let get path ?(params=[]) () =
  let query = Uri.encoded_of_query params in
  let uri = [ api_prefix_url; path; "?"; query; ] |> String.concat ""
            |> Uri.of_string in
  Clu.Client.get uri >>= fun (resp, body) ->

  let status = resp |> C.Response.status |> C.Code.string_of_status in
  Lwt_io.printlf "[GOT %s]\n--> %s" (Uri.to_string uri) status >>= fun () ->

  Lwt.return (resp, body)


let get_media media_id ?params () =
  get ("/media/" ^ media_id) ?params () >>= fun (resp, body) ->
  
  let status = resp |> C.Response.status in
  let code   = C.Code.code_of_status status in

  match C.Code.is_success code, status with
  | false, `Not_found -> Lwt.return None
  | false,          _ -> unexpected_response_status_exn resp body >>= raise
  | true,           _ -> 
    Clwt.Body.to_string body >>= fun body ->
    V2_media_body_j.t_of_string body |> Lwt.return_some

