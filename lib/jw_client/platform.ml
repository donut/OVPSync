
open Lwt.Infix
open Lib.Infix
open Printf
open Util

module Bopt = BatOption
module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix

type accounts_templates_list_body = Accounts_templates_list_body_t.t
type accounts_templates_list_template = Accounts_templates_list_body_t.template
type videos_conversions_list_body = Videos_conversions_list_body_t.t
type videos_conversions_list_conversion
  = Videos_conversions_list_body_t.conversion
type videos_list_body = Videos_list_body_t.t
type videos_list_video = Videos_video_t.t
type videos_show_body = Videos_show_body_t.t

type param = string * string list

let api_prefix_url = "https://api.jwplatform.com/v1"

module type Config = sig
  val key : string
  val secret : string
  val rate_limit_to_leave : int
end

module type Client = sig
  val call : string -> ?params : param list -> unit
             -> (C.Response.t * Clwt.Body.t) Lwt.t

  val accounts_templates_list : ?params : param list -> unit
                                    -> accounts_templates_list_body Lwt.t
  val videos_conversions_create : string -> string -> unit Lwt.t
  val videos_conversions_delete : string -> unit Lwt.t
  val videos_conversions_list : string -> videos_conversions_list_body Lwt.t
  val videos_list : ?params : param list -> unit -> videos_list_body Lwt.t
  val videos_show : string -> videos_show_body option Lwt.t
  val videos_update : string -> param list -> unit Lwt.t

  val create_conversion_by_name : string -> string -> unit Lwt.t
  val delete_conversion_by_name : string -> string -> unit Lwt.t
end


module Make (Log : Logger.Sig) (Conf : Config) : Client = struct

  let rate_limit_reset = ref 0.
  let rate_limit_remaining = ref 60 (* Default JW rate limit *)

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
    let signature = Sha1.(string (query ^ Conf.secret) |> to_hex) in
    ("api_signature", [signature]) :: params

  let call_count = ref 0
  let incr_call_count () =
    let current = !call_count in
    call_count := current + 1;
    current

  (** [call path ?params ()] Make a request to the endpoint at [path] with
      [params] as the query string. *)
  let rec call path ?(params=[]) () =
    (* Avoid using up the rate limit, leaving some for other applications *)
    let cnum = incr_call_count () in
    let now = Unix.time () in
    if now < !rate_limit_reset
        && !rate_limit_remaining <= Conf.rate_limit_to_leave then
      Log.infof "[%d] Leaving %d API hits; waiting %.0f seconds for reset."
        cnum !rate_limit_remaining (!rate_limit_reset -. now) >>= fun () ->
      Lwt_unix.sleep (!rate_limit_reset -. now) >>= fun () ->
      (* We call this function again to make sure the rate limit check is
         performed again. Otherwise, it may just assume it can continue
         and be wrong since there could be many other LWTs running requests
         at the same time. *)
      call path ~params ()
    else

    let params' = merge_params (gen_required_params ()) params in
    let signed = sign_query params' in
    let query = Uri.encoded_of_query signed in
    let uri_str = [ api_prefix_url; path; "?"; query; ] |> String.concat "" in
    let uri = Uri.of_string uri_str in

    let%lwt (resp, body) = try%lwt Clu.Client.get uri with
      | Unix.Unix_error(Unix.ETIMEDOUT, _, _) ->
        raise @@ Exn.Timeout ("GET", (Uri.to_string uri))
      | exn ->
        raise @@ Exn.Request_failure ("GET", (Uri.to_string uri), exn)
    in

    let status = resp |> C.Response.status in
    let status_str = status |> C.Code.string_of_status in
    Log.debugf "[%d] [GOT %s]\n--> %s" cnum uri_str status_str >>= fun () ->

    let h = resp |> C.Response.headers in
    rate_limit_reset := C.Header.get h "x-ratelimit-reset" 
      |> Bopt.map_default float_of_string_opt None =?: 0.;
    rate_limit_remaining := C.Header.get h "x-ratelimit-remaining"
      |> Bopt.map_default int_of_string_opt None =?: 0;

    Log.tracef "[%d] %d API hits remaining; Rate limit resets in %.0f seconds"
      cnum !rate_limit_remaining (!rate_limit_reset -. now) >>= fun () ->

    match status with
    | `Too_many_requests ->
      (* Wait and try again once the limit has reset *)
      let reset = BatFloat.max (!rate_limit_reset -. Unix.time ()) 1. in
      Log.warnf "[%d] Rate limit hit. Retrying in %.0f second(s)..." cnum reset
        >>= fun () ->
      Lwt_unix.sleep reset >>= fun () ->
      call path ~params ()
    | _ ->
      Lwt.return (resp, body)

  let accounts_templates_list ?params () =
    let path = "/accounts/templates/list" in
    let%lwt (resp, body) = call path ?params () in
    begin match C.Response.status resp with
    | `OK -> Lwt.return ()
    |   s ->
      Exn.unexpected_response_status ~path ?params ~resp ~body () >>= raise
    end >>= fun () ->
    let%lwt body' = Clwt.Body.to_string body in
    Lwt.return @@ Accounts_templates_list_body_j.t_of_string body'

  (** [get_videos_list ?params ()] Makes a request to the [/videos/list] 
      endpoint and returns the parsed response. *)
  let videos_list ?params () =
    let path = "/videos/list" in
    let%lwt (resp, body) = call path ?params () in
    match C.Response.status resp with
    | `OK ->
      let%lwt body' = Clwt.Body.to_string body in
      Lwt.return @@ Videos_list_body_j.t_of_string body'
    | s -> Exn.unexpected_response_status ~path ?params ~resp ~body () >>= raise

  let videos_show media_id =
    let path, params = "/videos/show", [("video_key", [media_id])] in
    let%lwt (resp, body) = call path ~params () in
    match C.Response.status resp with
    | `Not_found -> Lwt.return None
    | `OK ->
      let%lwt body' = Clwt.Body.to_string body in
      Lwt.return @@ Some (Videos_show_body_j.t_of_string body')
    | s -> Exn.unexpected_response_status ~path ~params ~resp ~body () >>= raise

  let videos_update key params =
    let path = "/videos/update" in
    let params = merge_params [("video_key", [key])] params in
    let%lwt (resp, body) = call path ~params () in

    match C.Response.status resp with
    | `OK -> Lwt.return ()
    | `Not_found -> raise Not_found
    |  s ->
      Exn.unexpected_response_status ~path ~params ~resp ~body () >>= raise

  let videos_conversions_create media_id template_key =
    let path = "/videos/conversions/create" in
    let params = 
      [ ("video_key", [media_id])
      ; ("template_key", [template_key]) ]
    in
    let%lwt (resp, body) = call path ~params () in
    match C.Response.status resp with
    | `OK 
    | `Conflict (* already exists *) -> Lwt.return ()
    | `Not_found -> raise Not_found
    | s -> Exn.unexpected_response_status ~path ~params ~resp ~body () >>= raise

  let videos_conversions_list media_id =
    let path = "/videos/conversions/list" in
    (* Leaving parameters `result_limit` and `result_offset` uncustomizable
     * as 1000 should way more than cover the possibilities for us at RTM. *)
    let params = [("video_key", [media_id]); ("result_limit", ["1000"])] in
    let%lwt (resp, body) = call path ~params () in
    match C.Response.status resp with
    | `OK ->
      let%lwt body' = Clwt.Body.to_string body in
      Lwt.return @@ Videos_conversions_list_body_j.t_of_string body'
    | `Not_found -> raise Not_found
    | s -> Exn.unexpected_response_status ~path ~params ~resp ~body () >>= raise

  let videos_conversions_delete key =
    let path, params =
      "/videos/conversions/delete",  [("conversion_key", [key])] in
    let%lwt (resp, body) = call path ~params () in
    match C.Response.status resp with
    | `OK -> Lwt.return ()
    | `Not_found -> raise Not_found
    | s -> Exn.unexpected_response_status ~path ~params ~resp ~body () >>= raise

  let create_conversion_by_name media_id template_name =
    let%lwt body = accounts_templates_list () in
    let name = String.lowercase_ascii template_name in
    let template = body.templates |> List.find begin fun t ->
      let open Accounts_templates_list_body_t in
      String.lowercase_ascii t.name = name
    end in
    videos_conversions_create media_id template.key
  
  let delete_conversion_by_name media_id template_name = 
    let%lwt body = videos_conversions_list media_id in
    let name = String.lowercase_ascii template_name in
    let conversion = body.conversions |> List.find begin fun c ->
      let open Videos_conversions_list_body_t in
      String.lowercase_ascii c.template.name = name
    end in
    videos_conversions_delete conversion.key

end
