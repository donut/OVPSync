
open Lwt.Infix


exception Unexpected_response_status of string * string * string

let unexpected_response_status_exn resp body =
  let status = resp |> Cohttp.Response.status |> Cohttp.Code.string_of_status in
  let headers = resp |> Cohttp.Response.headers |> Cohttp.Header.to_string in
  Cohttp_lwt.Body.to_string body >>= fun body' ->
  Lwt.return @@ Unexpected_response_status (status, headers, body')


(** [merge_params la lb] merges the parameters of [lb] into [la], replacing
    the values of any keys that already exists. *)
let merge_params la lb = 
  let la' = la |> List.filter (fun (ka, _) ->
    let exists = lb |> List.exists (fun (kb, _) -> kb == ka) in
    not exists
  ) in

  List.fold_left (fun l p -> p :: l) lb la'