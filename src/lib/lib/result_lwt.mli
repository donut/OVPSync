(** Encourages explicit error handling instead of raising exceptions in {!Lwt}
    async contexts.

    This is basically the {!type:result} monad wrapped in the {!Lwt.t} monad
    with the typical monad trappings along with the structure to work with
    [ppx_let]. It's nearly identical to {!module:Lwt_result} but has a few
    differences: the failure state is always {!type:exn} and it always checks
    {!type:Lwt.t} values in case they evaluate to {!type:exn} and moves that
    failure up into the {!type:result}. This way, to the extent this monad is
    used, all exceptions should be caught and rolled into the {!type:result}. 

    Opening {!Just_let_syntax} to use with [ppx_let].
    *)


type 'a t = ('a, exn) Base.Result.t Lwt.t


(** For use with [ppx_let]. Open {!Just_let_syntax} if you don't want
    everything at the top-level. *)
module Let_syntax : sig
  val return : 'a -> 'a t
  (** [return x] wraps [x] in {!type:t}. *)

  val bind : 'a t -> f : ('a -> 'b t) -> 'b t
  (** [bind x ~f] unwraps the value of [x] and passes it to [f], returning the
      result. If [x] evaluates to an error, nothing is done and [x] is returned
      as is. *)

  val map : 'a t -> f : ('a -> 'b) -> 'b t
  (** [map x ~f] unwraps the value of [x] and passes it to [f] and wraps the
      result back up in {!type:t}. If [x] evaluates to an error, nothing is
      done and [x] is returned as is. *)

  val both : 'a t -> 'b t -> ('a * 'b) t
  (** [both a b] combines the unwrapped values of [a] and [b] into the tuple 
      [(a, b)] and returns it wrapped up in {!type:t} unless [a] or [b] are 
      evaluate to an error, in which case the wrapped error of [a] (first) or
      [b] (second) is returned. *)

  module Open_on_rhs : sig
    val return : 'a -> 'a t
    (** [return x] wraps [x] in {!type:t}. *)

    val fail : exn -> 'a t
    (** [fail exn] wraps [exn] in {!type:}. *)

    val try_return : (unit -> 'a) -> 'a t
    (** [try_return f] runs [f ()] within a [try]. If no excepions are raised,
        the result is wrapped in {!type:t} and returned. Otherwise, caught
        exceptions are wrapped in {!type:t} as an error and returned. *)

    val return_lwt : 'a Lwt.t -> 'a t
    (** [return_lwt x] determines whether or not [x] evaluates to a value or an
        error and wraps it up appropriately in {!type:t}. *)

    val fail_lwt : exn Lwt.t -> 'a t
    (** [fail_lwt exn] unwraps the {!type:exn} value of [exn] from {!type:Lwt.t}
        and wraps it up as an error in {!type:t}. In case that [exn] evaluates
        to a failed {!Lwt.t} then that exception is wrapped up and returned 
        instead.  *)
  end
end


include module type of Let_syntax
include module type of Open_on_rhs


val to_lwt : 'a t -> 'a Lwt.t
(** [to_lwt t] unwraps the value of [t] and wraps it in {!type:Lwt.t} if it is
    [Ok] or a failed {!type:Lwt.t} if it is [Error]. *)


(** [open] this to use with [ppx_let] if you're not opening the top-level
    module. *)
module Just_let_syntax : sig
  module Let_syntax : module type of Let_syntax
end