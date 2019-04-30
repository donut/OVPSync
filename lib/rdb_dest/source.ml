
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

let items_are_same la lb =
  let have_same () =
    not @@ List.exists (fun x -> not @@ List.exists ((=) x) lb) la in
  List.compare_lengths la lb = 0 || have_same ()

let has_changed old knew =
  if not @@ are_same old knew then  
    let mkid x = x.name ^ " | " ^ x.media_id in
    raise @@ Sources_not_same (mkid old, mkid knew)
  else

  old.added <> knew.added
  || not (items_are_same old.custom knew.custom)