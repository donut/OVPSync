
type t = {
  id : int option;

  title : string;
  slug : string;
  
  created : int;
  updated : int;
  publish : int;
  expires : int option;

  file_uri : Uri.t option;
  filename : string;
  md5 : string option;
  size : int option; 
  (** The size of all transcodes as passed in from source. Not stored in DB. *)
  width : int option;
  height : int option;
  duration : int option;
  
  thumbnail_uri : Uri.t option;
  description : string option;
  tags : string list;
  custom : (string * string) list;

  cms_id : string option;
  link : Uri.t option;

  canonical : Source.t;
  sources : Source.t list;
} [@@deriving fields]