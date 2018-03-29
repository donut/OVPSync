
open Lwt.Infix
open Printf

module C = Cohttp
module Clwt = Cohttp_lwt
module Clu = Cohttp_lwt_unix

type videos_list_body = Videos_list_body_t.t
type video = Video_t.t

let platform_prefix_url = "https://api.jwplatform.com/v1"


let gen_required_params key =
  let timestamp = Unix.time () |> int_of_float |> string_of_int in
  let nonce = Random.int @@ BatInt.pow 10 8 |> sprintf "%08d" in

  [ "api_format", ["json"];
    "api_key", [key];
    "api_timestamp", [timestamp];
    "api_nonce", [nonce] ]


let merge_params la lb = 
  let la' = la |> List.filter (fun (ka, _) ->
    let exists = lb |> List.exists (fun (kb, _) -> kb == ka) in
    not exists
  ) in

  List.fold_left (fun l p -> p :: l) lb la'


let sign_query secret params =
  let query = params
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> Uri.encoded_of_query in
  let signature = Sha1.(string (query ^ secret) |> to_hex) in
  ("api_signature", [signature]) :: params


let call ~key ~secret path ?(params=[]) () =
  let params' = merge_params (gen_required_params key) params in
  let signed = sign_query secret params' in
  let query = Uri.encoded_of_query signed in
  let uri = [ platform_prefix_url; path; "?"; query; ] |> String.concat ""
            |> Uri.of_string in

  Clu.Client.get uri >>= fun (resp, body) ->

  let code = resp |> C.Response.status |> C.Code.code_of_status in
  printf "Response code: %d\n" code;
  printf "Headers: %s\n" (resp |> C.Response.headers |> C.Header.to_string);

  Lwt.return (resp, body)


let get_videos_list ~key ~secret ?params () =
  call ~key ~secret "/videos/list" ?params () >>= fun (_resp, body) ->
  Clwt.Body.to_string body >>= fun body ->
  Videos_list_body_j.t_of_string body |> Lwt.return


let get_videos_list_stream ~key ~secret ?(params=[]) ?(offset=0) () =
  let offset = ref offset in
  let videos = ref [] in

  Lwt_stream.from (fun () -> 
    (match !videos with 
      | [] -> 
        let params' = merge_params
          params
          ["result_offset", [!offset |> string_of_int]] in
        get_videos_list ~key ~secret ~params:params' () >>= fun body ->
        offset := body.offset;
        Lwt.return body.videos

      | l -> Lwt.return l

    ) >>= function
    | [] -> Lwt.return None
    | h :: tl ->
      videos := tl;
      offset := !offset + 1;
      Lwt.return @@ Some (!offset, h)
  )
