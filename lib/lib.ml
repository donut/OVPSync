
module Conf = Lib_conf

module Infix = struct
  (** [l <% r] composes the functions [l] and [r]. The result is a function
      that will accept [x] and pass it to [r], taking the result of that and 
      passing it to [l]. *)
  let ( <% ) l r x = x |> r |> l

  (** [l %> r] composes the functions [l] and [r]. The result is a function
      that will accept [x] and pass it to [l], taking the result of that and 
      passing it to [r]. *)
  let ( %> ) l r x = x |> l |> r
  
  (** [o =?: d] returns the value of [o] unless it is [None], then returns 
      [d]. *)
  let ( =?: ) o d = match o with None -> d | Some v -> v

  (** [o >>? f] applies the value of [o] to the function [f] if [o] is not  
      [None]. *)
  let ( >>? ) o f = match o with None -> None | Some v -> f v

  (** [o >|? f] applies the value of [o] to the function [f] and wraps it in
      [Some] if [o] is not [None]. *)
  let ( >|? ) o f = match o with None -> None | Some v -> Some (f v) 
end