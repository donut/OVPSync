

let spf = Printf.sprintf


let to_string exn =
  match exn with
  | Remote_file.Timeout { meth; uri }
  | Jw_client.Exn.Timeout (meth, uri) ->
    spf "Request timed out: [%s %s]" meth uri

  | Jw_client.Exn.Unexpected_response_status (method_, path, response) ->
    spf
      "Unexpected HTTP response\n\
        --> Request:\n[%s %s]\n\n\
        --> Response <--\n%s\n\n"
      method_ path response
    
  | Remote_file.Request_failure { meth; uri; exn } ->
    let exn = Printexc.to_string exn in
    spf "Request failure [%s %s]: %s" meth uri exn

  | Jw_client.Exn.Temporary_error (meth, uri, exn) ->
    spf "Temporary error making request [%s %s]: %s" meth uri exn

  | exn ->
    spf "Unexpected error: %s" (Printexc.to_string exn)