
module Conf = Lib_conf

module Infix = struct
  (** [l <% r] composes the functions [l] and [r]. The result is a function
      that will accept [x] and pass it to [r], passing the result of that to 
      [l]. *)
  let ( <% ) l r x = x |> r |> l

  (** [l %> r] composes the functions [l] and [r]. The result is a function
      that will accept [x] and pass it to [l], passing the result of that to 
      [r]. *)
  let ( %> ) l r x = x |> l |> r

  (** [l $@ r] takes the function [l] and returns a new function that will
      accept [x], passing it to [l] as the first argument and [r] as the 
      second. *)
  let ( $@ ) l r x = l x r

  (** [l & r] allows multiple arguments to be placed ahead of the first 
      argument in [l]. Can be used as [(f $@ a & b & c) d] with the resulting 
      call being [f d a b c]. *)
  let ( & ) l r = l $@ r
  
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