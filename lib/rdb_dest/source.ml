
type t = {
  id : int option;
  name : string;
  media_id : string;
  video_id : int option;
  added : int; (* When the source was added at the OVP *)
  modified : int; (* When the source was last modified at the OVP *)
  custom : (string * string) list;
} [@@deriving fields]

let are_same a b =
  name a = name b && media_id a = media_id b

exception Sources_not_same of string * string

let has_changed old knew =
  if not @@ are_same old knew then  
    let mkid x = x.name ^ " | " ^ x.media_id in
    raise @@ Sources_not_same (mkid old, mkid knew)
  else

  old.added <> knew.added
  || Util.fields_have_changed old.custom knew.custom