
module Conf = Lib_conf

module Infix = struct
  let ( <% ) l r x = x |> r |> l
  let ( %> ) l r x = x |> l |> r
  let ( =?: ) o d = match o with None -> d | Some v -> v
  let ( >>? ) o f = match o with None -> None | Some v -> f v
  let ( >|? ) o f = match o with None -> None | Some v -> Some (f v) 
end