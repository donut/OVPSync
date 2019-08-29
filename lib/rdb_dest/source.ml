
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


let rec items_are_same la lb =
  if List.compare_lengths la lb <> 0 then false
  else

  match la with
  | [] -> BatList.is_empty lb
  | hd :: tl ->
    let filter = List.filter ((<>) hd) in
    items_are_same (filter tl) (filter lb)


let has_changed old knew =
  if not @@ are_same old knew then  
    let mkid x = x.name ^ " | " ^ x.media_id in
    raise @@ Sources_not_same (mkid old, mkid knew)
  else

  old.added <> knew.added
  || not (items_are_same old.custom knew.custom)