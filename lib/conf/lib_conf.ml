

module Root = Root_t
module Jw_client = Jw_client_t
module Jw_source = Jw_source_t
module Rdb_dest = Rdb_dest_t
module Sync = Sync_t

let read_file path =
  BatFile.lines_of path |> BatList.of_enum |> String.concat ""

let read_config path = read_file path |> Root_j.t_of_string

let log_level (root : Root.t) (lvl : Log_level_t.t) =
  match root.log_level_override with
  | Some l -> l
  | None -> lvl
