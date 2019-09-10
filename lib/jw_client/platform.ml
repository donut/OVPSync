

open Exn
(* Must be opened before Base, since base will replace the Exn module. *)

open Base
open Printf

open Lib.Infix.Float
open Lib.Infix.Option
open Lib.Lwt_result

open Util


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
  val call 
     : string -> ?params : param list -> unit
    -> (Cohttp.Response.t * Cohttp_lwt.Body.t, exn) Lib.Lwt_result.t

  val accounts_templates_list 
     : ?params : param list -> unit
    -> (accounts_templates_list_body, exn) Lib.Lwt_result.t

  val videos_conversions_create 
     : string -> string 
    -> (unit, exn) Lib.Lwt_result.t

  val videos_conversions_delete : string -> (unit, exn) Lib.Lwt_result.t

  val videos_conversions_list 
    : string -> (videos_conversions_list_body, exn) Lib.Lwt_result.t

  val videos_list 
     : ?params : param list -> unit 
    -> (videos_list_body, exn) Lib.Lwt_result.t

  val videos_show 
     : string 
    -> (videos_show_body option, exn) Lib.Lwt_result.t

  val videos_update 
     : string -> param list 
    -> (unit, exn) Lib.Lwt_result.t

  val create_conversion_by_name 
     : string -> string 
    -> (unit, exn) Lib.Lwt_result.t

  val delete_conversion_by_name 
     : string -> string 
    -> (unit, exn) Lib.Lwt_result.t
end


module Make (Log : Logger.Sig) (Conf : Config) : Client = struct
  let rate_limit_reset = ref 0.
  let rate_limit_remaining = ref 60 (* Default JW rate limit *)


  (** [gen_required_params ()] generates the required parameters for an API
      call *)
  let gen_required_params () =
    let timestamp = Unix.time () |> Int.of_float |> Int.to_string in
    let nonce = Random.int @@ BatInt.pow 10 8 |> sprintf "%08d" in

    [ "api_format", ["json"];
      "api_key", [Conf.key];
      "api_timestamp", [timestamp];
      "api_nonce", [nonce] ]


  (** [sign_query params] generates a signatures based on the passed parameters
      and returns a new list of params with the signature *)
  let sign_query params =
    let query = params
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
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
    
    if now <. !rate_limit_reset
        && !rate_limit_remaining <= Conf.rate_limit_to_leave then
      let%lwt () = Log.infof
        "[%d] Leaving %d API hits; waiting %.0f seconds for reset."
        cnum !rate_limit_remaining (!rate_limit_reset -. now)
      in
      let%lwt () = Lwt_unix.sleep (!rate_limit_reset -. now) in
      (* We call this function again to make sure the rate limit check is
         performed again. Otherwise, it may just assume it can continue
         and be wrong since there could be many other LWTs running requests
         at the same time. *)
      call path ~params ()
    else

    let params' = merge_params (gen_required_params ()) params in
    let signed = sign_query params' in
    let query = Uri.encoded_of_query signed in
    let uri_str = [ api_prefix_url; path; "?"; query; ] |> String.concat in
    let uri = Uri.of_string uri_str in

    let%bind (resp, body) =
      try%lwt 
        return_lwt @@ Clu.Client.get uri
      with
      | Unix.Unix_error (Unix.ETIMEDOUT, _, _) ->
        Lwt.return @@ Error (Timeout ("GET", (Uri.to_string uri)))
      | exn ->
        Lwt.return @@ Error (unknown_request_failure "GET" uri exn)
    in

    let status = resp |> C.Response.status in
    let status_str = status |> C.Code.string_of_status in
    let%lwt () = Log.debugf "[%d] [GOT %s]\n--> %s" cnum uri_str status_str in

    let () =
      let h = resp |> C.Response.headers in

      rate_limit_reset :=
      C.Header.get h "x-ratelimit-reset" 
      |> Option.value_map ~f:Caml.float_of_string_opt ~default:None
      =?: 0.;

      rate_limit_remaining :=
      C.Header.get h "x-ratelimit-remaining"
      |> Option.value_map ~f:Caml.int_of_string_opt ~default:None
      =?: 0
    in

    let%lwt () = Log.tracef
      "[%d] %d API hits remaining; Rate limit resets in %.0f seconds"
      cnum !rate_limit_remaining (!rate_limit_reset -. now) in

    match status with
    | `Too_many_requests ->
      (* Wait and try again once the limit has reset *)
      let reset = BatFloat.max (!rate_limit_reset -. Unix.time ()) 1. in
      let%lwt () = Log.warnf
        "[%d] Rate limit hit. Retrying in %.0f second(s)..." cnum reset in
      let%lwt () = Lwt_unix.sleep reset in
      call path ~params ()

    | _ ->
      Lwt.return @@ Ok (resp, body)


  let accounts_templates_list ?params () =
    let path = "/accounts/templates/list" in
    let%bind (resp, body) = call path ?params () in

    match C.Response.status resp with
    | `OK ->
      let%lwt body = Clwt.Body.to_string body in
      Lwt_result.return @@ Accounts_templates_list_body_j.t_of_string body

    |   _ ->
      fail_lwt @@ unexpected_response_status ~path ?params ~resp ~body () 


  (** [get_videos_list ?params ()] Makes a request to the [/videos/list] 
      endpoint and returns the parsed response. *)
  let videos_list ?params () =
    let path = "/videos/list" in
    let%bind resp, body = call path ?params () in

    match C.Response.status resp with
    | `OK ->
      let%lwt body = Clwt.Body.to_string body in
      return @@ Videos_list_body_j.t_of_string body

    | _ -> 
      fail_lwt @@ unexpected_response_status ~path ?params ~resp ~body ()


  let videos_show media_id =
    let path, params = "/videos/show", [("video_key", [media_id])] in
    let%bind resp, body = call path ~params () in

    match C.Response.status resp with
    | `Not_found -> 
      return None

    | `OK ->
      let%lwt body = Clwt.Body.to_string body in
      return @@ Some (Videos_show_body_j.t_of_string body)

    | _ ->
      fail_lwt @@ unexpected_response_status ~path ~params ~resp ~body ()


  let videos_update key params =
    let path = "/videos/update" in
    let params = merge_params [("video_key", [key])] params in
    let%bind resp, body = call path ~params () in

    match C.Response.status resp with
    | `OK ->
      return ()

    | `Not_found ->
      fail @@ Not_found_s (Parsexp.Single.parse_string_exn key)

    |  _ ->
      fail_lwt @@ unexpected_response_status ~path ~params ~resp ~body ()


  let videos_conversions_create media_id template_key =
    let path = "/videos/conversions/create" in
    let params = 
      [ ("video_key", [media_id])
      ; ("template_key", [template_key]) ]
    in

    let%bind resp, body = call path ~params () in

    match C.Response.status resp with
    | `OK 
    | `Conflict (* already exists *) ->
      return ()

    | `Not_found ->
      fail @@ Not_found_s (Parsexp.Single.parse_string_exn media_id)

    | _ -> 
      fail_lwt @@ unexpected_response_status ~path ~params ~resp ~body ()


  let videos_conversions_list media_id =
    let path = "/videos/conversions/list" in
    (* Leaving parameters `result_limit` and `result_offset` uncustomizable
     * as 1000 should way more than cover the possibilities for us at RTM. *)
    let params = [("video_key", [media_id]); ("result_limit", ["1000"])] in

    let%bind resp, body = call path ~params () in
    match C.Response.status resp with
    | `OK ->
      let%lwt body = Clwt.Body.to_string body in
      return @@ Videos_conversions_list_body_j.t_of_string body

    | `Not_found ->
      fail @@ Not_found_s (Parsexp.Single.parse_string_exn media_id)

    | _ ->
      fail_lwt @@ unexpected_response_status ~path ~params ~resp ~body ()


  let videos_conversions_delete key =
    let path, params =
      "/videos/conversions/delete",  [("conversion_key", [key])] in
    let%bind resp, body = call path ~params () in

    match C.Response.status resp with
    | `OK ->  
      return ()

    | `Not_found ->
      fail @@ Not_found_s (Parsexp.Single.parse_string_exn key)

    | _ ->
      fail_lwt @@ unexpected_response_status ~path ~params ~resp ~body ()


  let create_conversion_by_name media_id template_name =
    let open Accounts_templates_list_body_t in

    let%bind { templates; _ } = accounts_templates_list () in
    let name = String.lowercase template_name in

    templates
    |> List.find ~f:(fun t -> String.(equal (lowercase t.name) name))
    >|? (fun { key; _ } -> videos_conversions_create media_id key)
    =?: fail @@ Not_found_s (Parsexp.Single.parse_string_exn template_name)
    

  let delete_conversion_by_name media_id template_name = 
    let open Videos_conversions_list_body_t in

    let%bind body = videos_conversions_list media_id in
    let name = String.lowercase template_name in
    
    body.conversions
    |> List.find
      ~f:(fun c -> String.(lowercase c.template.name |> equal name))
    >|? (fun { key; _ } -> videos_conversions_delete key)
    =?: fail @@ Not_found_s (Parsexp.Single.parse_string_exn template_name)
end
