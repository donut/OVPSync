
open Base 

module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix

module Result_lwt = Lib.Result_lwt
open Result_lwt.Just_let_syntax


exception Timeout of { meth : string; uri : string }
exception Request_failure of { meth : string; uri : string; exn : exn }


let rec get_uri ?(redirects=30) uri =
  let%bind_open (resp, body) =
    match%lwt Clu.Client.get uri |> return_lwt with
    | Error (Unix.Unix_error(Unix.ETIMEDOUT, _, _)) ->
      fail @@ Timeout { meth = "GET"; uri = Uri.to_string uri }

    | Error exn ->
      fail @@ Request_failure { meth = "GET"; uri = Uri.to_string uri; exn }

    | Ok v ->
      return v
  in

  (* Cut off redirect loop. *)
  if redirects = 0
  then Result_lwt.return (resp, body)
  else

  match C.Response.status resp with
  | `Found | `Moved_permanently | `See_other | `Temporary_redirect ->
    begin match C.Header.get (C.Response.headers resp) "location" with
    | None -> Result_lwt.return (resp, body)
    | Some l -> get_uri ~redirects:(redirects - 1) (Uri.of_string l)
    end

  | _ ->
    Result_lwt.return (resp, body)


let test uri =
  let result = get_uri uri in
  let%bind (resp, body) = result in

  if Poly.equal (C.Response.status resp) `OK then
    (* Hacky way to close the connection without dowloading the response body.
       Was not able to find any better solutions. Opened a
       {{: https://github.com/mirage/ocaml-cohttp/issues/674} case with {!module:Cohttp}}.  *)
    let () = Lwt.cancel result in
    Result_lwt.return ()

  else
    Jw_client.Exn.unexpected_response_status
      ~path:(Uri.to_string uri) ~resp ~body ()
    |> Result_lwt.fail_lwt
