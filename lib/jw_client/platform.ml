
open Lwt.Infix
open Printf
open Util

module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix

type videos_list_body = Videos_list_body_t.t
type videos_list_video = Videos_list_body_t.video
type accounts_templates_list_body = Accounts_templates_list_body_t.t
type accounts_templates_list_template = Accounts_templates_list_body_t.template
type param = string * string list

let api_prefix_url = "https://api.jwplatform.com/v1"

module type Config = sig
  val key : string
  val secret : string
end

module type Client = sig
  val call : string -> ?params : param list -> unit
             -> (C.Response.t * Clwt.Body.t) Lwt.t
  val get_accounts_templates_list : ?params : param list -> unit
                                    -> accounts_templates_list_body Lwt.t
  val get_videos_list : ?params : param list -> unit -> videos_list_body Lwt.t
  val videos_update : string -> param list -> unit Lwt.t
  val videos_conversions_create : string -> string -> unit Lwt.t
  val create_conversion_by_name : string -> string -> unit Lwt.t
end


module Make (Conf : Config) : Client = struct

  (** [gen_required_params ()] generates the required parameters for an API
      call *)
  let gen_required_params () =
    let timestamp = Unix.time () |> int_of_float |> string_of_int in
    let nonce = Random.int @@ BatInt.pow 10 8 |> sprintf "%08d" in

    [ "api_format", ["json"];
      "api_key", [Conf.key];
      "api_timestamp", [timestamp];
      "api_nonce", [nonce] ]


  (** [sign_query params] generates a signatures based on the passed parameters
      and returns a new list of params with the signature *)
  let sign_query params =
    let query = params
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      |> Uri.encoded_of_query 
      (* Catch extra characters expected to be encoded by JW *)
      |> BatString.replace_chars (function
        | ':' -> "%3A" 
        | c -> BatString.of_char c)
    in
    print_endline @@ sprintf "[%s]" query;
    let signature = Sha1.(string (query ^ Conf.secret) |> to_hex) in
    ("api_signature", [signature]) :: params


  (** [call path ?params ()] Make a request to the endpoint at [path] with
      [params] as the query string. *)
  let rec call path ?(params=[]) () =
    let params' = merge_params (gen_required_params ()) params in
    let signed = sign_query params' in
    let query = Uri.encoded_of_query signed in
    let uri_str = [ api_prefix_url; path; "?"; query; ] |> String.concat "" in
    let uri = Uri.of_string uri_str in

    Lwt_io.printlf "[GET %s]" uri_str >>= fun () ->
    Clu.Client.get uri >>= fun (resp, body) ->

    let status = resp |> C.Response.status in
    let status_str = status |> C.Code.string_of_status in
    Lwt_io.printlf "--> Response status: %s" status_str >>= fun () ->

    (* Lwt_io.printlf "Headers: %s" (resp |> C.Response.headers |> C.Header.to_string)
    >>= fun () -> *)

    match status with
    | `Too_many_requests ->
      (* Wait and try again once the limit has reset *)
      let h = resp |> C.Response.headers in
      let reset =  match C.Header.get h "x-ratelimit-reset" with
        | None -> 60.
        | Some r -> match float_of_string_opt r with
          | None -> 60.
          | Some r -> BatFloat.max (r -. Unix.time ()) 1.
      in
      Lwt_io.printlf "--> Rate limit hit. Retrying in %f.1 second(s)..." reset
      >>= fun () ->
      Lwt_unix.sleep reset >>= fun () ->
      call path ~params ()
    | _ ->
      Lwt.return (resp, body)

  let get_accounts_templates_list ?params () =
    call "/accounts/templates/list" ?params () >>= fun (resp, body) ->
    begin match C.Response.status resp with
    | `OK -> Lwt.return ()
    |   s -> unexpected_response_status_exn resp body >>= raise
    end >>= fun () ->
    Clwt.Body.to_string body >>= fun body ->
    Accounts_templates_list_body_j.t_of_string body |> Lwt.return

  (** [get_videos_list ?params ()] Makes a request to the [/videos/list] 
      endpoint and returns the parsed response. *)
  let get_videos_list ?params () =
    call "/videos/list" ?params () >>= fun (_resp, body) ->
    Clwt.Body.to_string body >>= fun body ->
    Videos_list_body_j.t_of_string body |> Lwt.return

  let videos_update key params =
    let params' = merge_params [("video_key", [key])] params in
    call "/videos/update" ~params:params' () >>= fun (resp, body) ->

    match C.Response.status resp with
    | `OK -> Lwt.return ()
    |   s -> unexpected_response_status_exn resp body >>= raise

  let videos_conversions_create media_id template_key =
    let params = 
      [ ("video_key", [media_id]);
        ("template_key", [template_key]) ]
    in
    call "/videos/conversions/create" ~params () >>= fun (resp, body) ->
    match C.Response.status resp with
    | `OK 
    | `Conflict (* already exists *) -> Lwt.return ()
    | s -> unexpected_response_status_exn resp body >>= raise


  let create_conversion_by_name media_id template_name =
    get_accounts_templates_list () >>= fun body ->
    let name = String.lowercase_ascii template_name in
    let template = body.templates |> List.find begin fun t ->
      let open Accounts_templates_list_body_t in
      String.lowercase_ascii t.name = name
    end in
    videos_conversions_create media_id template.key
end
