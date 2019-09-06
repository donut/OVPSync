(** Interface to JW's Delivery API.

    {{: https://developer.jwplayer.com/jw-platform/docs/delivery-api-reference/} Documentation}
    *)


type v2_media_body = V2_media_body_t.t
(** Response type of /v2/media endpoint. *)


val get 
   : string -> ?params:(string * string list) list -> unit
  -> (Cohttp.Response.t * Cohttp_lwt.Body.t, exn) Common.Lwt_result.t
(** [get path ?params ()] makes a GET request to the JW Delivery API endpoint
    specified by [path]. *)

val get_media
   : string -> ?params:(string * string list) list -> unit
  -> (v2_media_body option, exn) Common.Lwt_result.t
(** [get_media media_id ?params ()] returns the response from the [/v2/media] 
    endpoint for the given [media_id].
    
    {{: https://developer.jwplayer.com/jw-platform/docs/delivery-api-reference/#/Media/get_v2_media__media_id_} Documentation}
    *)
