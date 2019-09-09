

type ('ok, 'bad) t = ('ok, 'bad) Base.Result.t Lwt.t

val return : 'ok -> ('ok, 'bad) t
val fail : 'bad -> ('ok, 'bad) t
val return_lwt : 'ok Lwt.t -> ('ok, 'bad) t
val fail_lwt : 'bad Lwt.t -> ('ok, 'bad) t

module Let_syntax : sig
  val return : 'ok -> ('ok, 'bad) t
  val bind : ('a, 'bad) t -> f : ('a -> ('b, 'bad) t) -> ('b, 'bad) t
  val map : ('a, 'bad) t -> f : ('a -> 'b) -> ('b, 'bad) t
  val both : ('a, 'bad) t -> ('b, 'bad) t -> ('a * 'b, 'bad) t

  module Open_on_rhs : sig
    val return : 'ok -> ('ok, 'bad) t
    val fail : 'bad -> ('ok, 'bad) t
    val return_lwt : 'ok Lwt.t -> ('ok, 'bad) t
    val fail_lwt : 'bad Lwt.t -> ('ok, 'bad) t
  end
end

module Just_let_syntax : sig
  module Let_syntax = Let_syntax
end