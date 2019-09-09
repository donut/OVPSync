

module Function = struct
  let ( <% ) l r x = x |> r |> l

  let ( %> ) l r x = x |> l |> r

  let ( $@ ) l r x = l x r

  let ( & ) l r = l $@ r
end


module Option = struct
  let ( =?: ) o d = match o with None -> d | Some v -> v

  let ( >>? ) o f = match o with None -> None | Some v -> f v

  let ( >|? ) o f = match o with None -> None | Some v -> Some (f v) 
end


module Float = struct
  let (  <. ) l r = Float.compare l r < 0
  let ( <=. ) l r = Float.compare l r <= 0
  let (  >. ) l r = Float.compare l r > 0
  let ( >=. ) l r = Float.compare l r >= 0
  let (  =. )     = Float.equal
end