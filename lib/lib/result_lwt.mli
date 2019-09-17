

type 'ok t = ('ok, exn) Base.Result.t Lwt.t


module Let_syntax : sig
  val return : 'ok -> 'ok t
  val bind : 'a t -> f : ('a -> 'b t) -> 'b t
  val map : 'a t -> f : ('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t

  module Open_on_rhs : sig
    val return : 'ok -> 'ok t
    val fail : exn -> 'ok t
    val try_return : (unit -> 'ok) -> 'ok t
    val return_lwt : 'ok Lwt.t -> 'ok t
    val fail_lwt : exn Lwt.t -> 'ok t
  end
end


include module type of Let_syntax
include module type of Open_on_rhs


module Just_let_syntax : sig
  module Let_syntax : module type of Let_syntax
end