

type ('ok, 'bad) t = ('ok, 'bad) Base.Result.t Lwt.t

module Let_syntax : sig
  module Open_on_rhs : sig
    val return : 'ok -> ('ok, 'bad) t
    val fail : 'bad -> ('ok, 'bad) t
    val try_return : (unit -> 'ok) -> ('ok, exn) t
    val return_lwt : 'ok Lwt.t -> ('ok, 'bad) t
    val fail_lwt : 'bad Lwt.t -> ('ok, 'bad) t
  end

  val return : 'ok -> ('ok, 'bad) t
  val bind : ('a, 'bad) t -> f : ('a -> ('b, 'bad) t) -> ('b, 'bad) t
  val map : ('a, 'bad) t -> f : ('a -> 'b) -> ('b, 'bad) t
  val both : ('a, 'bad) t -> ('b, 'bad) t -> ('a * 'b, 'bad) t
end


include module type of Let_syntax
include module type of Open_on_rhs


module Just_let_syntax : sig
  module Let_syntax : module type of Let_syntax
end