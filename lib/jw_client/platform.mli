

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
    -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lib.Result_lwt.t

  val accounts_templates_list 
     : ?params : param list -> unit
    -> accounts_templates_list_body Lib.Result_lwt.t

  val videos_conversions_create 
     : string -> string 
    -> unit Lib.Result_lwt.t

  val videos_conversions_delete : string -> unit Lib.Result_lwt.t

  val videos_conversions_list 
    : string -> videos_conversions_list_body Lib.Result_lwt.t

  val videos_list 
     : ?params : param list -> unit 
    -> videos_list_body Lib.Result_lwt.t

  val videos_show 
     : string 
    -> videos_show_body option Lib.Result_lwt.t

  val videos_update 
     : string -> param list 
    -> unit Lib.Result_lwt.t

  val create_conversion_by_name 
     : string -> string 
    -> unit Lib.Result_lwt.t

  val delete_conversion_by_name 
     : string -> string 
    -> unit Lib.Result_lwt.t
end


module Make (Log : Logger.Sig) (Conf : Config) : Client