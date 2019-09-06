

type accounts_templates_list_body = Accounts_templates_list_body_t.t
type accounts_templates_list_template = Accounts_templates_list_body_t.template
type videos_conversions_list_body = Videos_conversions_list_body_t.t
type videos_conversions_list_conversion
  = Videos_conversions_list_body_t.conversion
type videos_list_body = Videos_list_body_t.t
type videos_list_video = Videos_video_t.t
type videos_show_body = Videos_show_body_t.t

type param = string * string list


module type Config = sig
  val key : string
  val secret : string
  val rate_limit_to_leave : int
end


module type Client = sig
  val call 
     : string -> ?params : param list -> unit
    -> (Cohttp.Response.t * Cohttp_lwt.Body.t, exn) Common.Lwt_result.t

  val accounts_templates_list 
     : ?params : param list -> unit
    -> (accounts_templates_list_body, exn) Common.Lwt_result.t

  val videos_conversions_create 
     : string -> string 
    -> (unit, exn) Common.Lwt_result.t

  val videos_conversions_delete : string -> (unit, exn) Common.Lwt_result.t

  val videos_conversions_list 
    : string -> (videos_conversions_list_body, exn) Common.Lwt_result.t

  val videos_list 
     : ?params : param list -> unit 
    -> (videos_list_body, exn) Common.Lwt_result.t

  val videos_show 
     : string 
    -> (videos_show_body option, exn) Common.Lwt_result.t

  val videos_update 
     : string -> param list 
    -> (unit, exn) Common.Lwt_result.t

  val create_conversion_by_name 
     : string -> string 
    -> (unit, exn) Common.Lwt_result.t

  val delete_conversion_by_name 
     : string -> string 
    -> (unit, exn) Common.Lwt_result.t
end