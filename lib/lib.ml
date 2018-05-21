
module Conf = Lib_conf

module Infix = struct
  let ( <% ) l r x = x |> r |> l
  let ( %> ) l r x = x |> l |> r
  let ( =?: ) o d = match o with None -> d | Some v -> v
end