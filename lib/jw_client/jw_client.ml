
open Lwt.Infix
open Printf

module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix

type videos_list_body = Videos_list_body_t.t
type video = Video_t.t
type param = string * string list

let platform_prefix_url = "https://api.jwplatform.com/v1"

(** [merge_params la lb] merges the parameters of [lb] into [la], replacing
    the values of any keys that already exists. *)
let merge_params la lb = 
  let la' = la |> List.filter (fun (ka, _) ->
    let exists = lb |> List.exists (fun (kb, _) -> kb == ka) in
    not exists
  ) in

  List.fold_left (fun l p -> p :: l) lb la'

module type Config = sig
  val key : string
  val secret : string
end

module type Client = sig
  val call : string -> ?params : param list -> unit
             -> (C.Response.t * Clwt.Body.t) Lwt.t
  val get_videos_list : ?params : param list -> unit -> videos_list_body Lwt.t
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
      |> Uri.encoded_of_query in
    let signature = Sha1.(string (query ^ Conf.secret) |> to_hex) in
    ("api_signature", [signature]) :: params


  (** [call path ?params ()] Make a request to the endpoint at [path] with
      [params] as the query string. *)
  let call path ?(params=[]) () =
    let params' = merge_params (gen_required_params ()) params in
    let signed = sign_query params' in
    let query = Uri.encoded_of_query signed in
    let uri = [ platform_prefix_url; path; "?"; query; ] |> String.concat ""
              |> Uri.of_string in

    Clu.Client.get uri >>= fun (resp, body) ->

    let code = resp |> C.Response.status |> C.Code.code_of_status in
    printf "Response code: %d\n" code;
    printf "Headers: %s\n" (resp |> C.Response.headers |> C.Header.to_string);

    Lwt.return (resp, body)


  (** [get_videos_list ?params ()] Makes a request to the [/videos/list] 
      endpoint and returns the parsed response. *)
  let get_videos_list ?params () =
    call "/videos/list" ?params () >>= fun (_resp, body) ->
    Clwt.Body.to_string body >>= fun body ->
    Videos_list_body_j.t_of_string body |> Lwt.return

end
