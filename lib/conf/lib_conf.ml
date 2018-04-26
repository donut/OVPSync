
let read_file filename =
  BatFile.lines_of filename |> BatList.of_enum |> String.concat ""

module JW_source = struct
  include Jw_source_t
  let of_string = Jw_source_j.t_of_string
  let key t = t.key
  let secret t = t.secret
  let rate_limit_to_leave t = t.rate_limit_to_leave
end

