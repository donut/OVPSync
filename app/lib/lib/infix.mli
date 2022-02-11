
module Function : sig
  val ( <| ) : ('a -> 'b) -> 'a -> 'b
  (** [l <| r] passes [r] to [l]. Basically, the opposite of [|>]. *)

  val ( <% ) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
  (** [l <% r] composes the functions [l] and [r]. The result is a function
      that will accept [x] and pass it to [r], passing the result of that to 
      [l]. *)

  val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
  (** [l %> r] composes the functions [l] and [r]. The result is a function
      that will accept [x] and pass it to [l], passing the result of that to 
      [r]. *)

  val ( $@ ) : ('a -> 'b -> 'c) -> 'b -> ('a -> 'c)
  (** [l $@ r] takes the function [l] and returns a new function that will
      accept [x], passing it to [l] as the first argument and [r] as the 
      second. For example [(f $@ a) b] will result in the call [f b a].*)

  val ( & ) : ('a -> 'b -> 'c) -> 'b -> ('a -> 'c)
  (** [l & r] allows multiple arguments to be placed ahead of the first 
      argument in [l]. Can be used as [(f $@ a & b & c) d] with the resulting 
      call being [f d a b c]. *)
end


module Option : sig
  val ( =?: ) : 'a option -> 'a -> 'a
  (** [o =?: d] returns the value of [o] unless it is [None], then returns 
      [d]. *)

  val ( >>? ) : 'a option -> ('a -> 'b option) -> 'b option
  (** [o >>? f] applies the value of [o] to the function [f] if [o] is not  
      [None]. *)

  val ( >|? ) : 'a option -> ('a -> 'b) -> 'b option
  (** [o >|? f] applies the value of [o] to the function [f] and wraps it in
      [Some] if [o] is not [None]. *)
end


module Float : sig
  val (  <. ) : float -> float -> bool
  val ( <=. ) : float -> float -> bool
  val (  >. ) : float -> float -> bool
  val ( >=. ) : float -> float -> bool
  val (  =. ) : float -> float -> bool
end