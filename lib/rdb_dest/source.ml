
type t = {
  name: string;
  id: string;
  created: int;
  updated: int;
  custom: (string * string) list;
} [@@deriving fields]