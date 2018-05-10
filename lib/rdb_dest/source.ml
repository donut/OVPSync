
type t = {
  id : int option;
  name : string;
  media_id : string;
  video_id : int option;
  added : int; (* When the source was added at the OVP *)
  modified : int; (* When the source was last modified at the OVP *)
  custom : (string * string) list;
} [@@deriving fields]