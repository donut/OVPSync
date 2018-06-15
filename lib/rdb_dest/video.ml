
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

let sources_have_changed old knew =
  knew |> List.exists (fun k ->
    match List.find_opt (Source.are_same k) old with
    | None -> true
    | Some o -> Source.has_changed o k)

let has_changed old knew =
  (* At first I tried just comparing update/modified timestamps and the MD5
     of the video file, but because the source may do clean up on the item
     after giving it to the destination, these timestamps are likely to change. 
  *)
     old.title <> knew.title
  || old.slug <> knew.slug
  || old.publish <> knew.publish
  || old.expires <> knew.expires
  (* file_uri and thumbnail_uri are not checked as they are changed to the
     local URI when they're saved *)
  || old.filename <> knew.filename
  || old.md5 <> knew.md5
  || old.width <> knew.width
  || old.height <> knew.height
  || old.duration <> knew.duration
  || old.description <> knew.description
  || old.tags <> knew.tags
  || old.cms_id <> knew.cms_id
  || old.link <> knew.link
  || Util.fields_have_changed old.custom knew.custom
  || not (Source.are_same old.canonical knew.canonical)
  || Source.has_changed old.canonical knew.canonical
  || sources_have_changed old.sources knew.sources
