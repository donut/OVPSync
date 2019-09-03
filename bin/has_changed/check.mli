
type ('value, 'result) runner 
   = has_changed:('value -> 'value -> bool)
  -> to_log:('value -> 'value -> string)
  -> 'value
  -> 'value
  -> 'result


module type Config = sig
  type result
  val runner : ('value, result) runner
end


module Make : functor (Conf : Config) -> sig
  type result = Conf.result

  val int_field : string -> old:int -> new':int -> result

  val uri_field : string -> old:Uri.t -> new':Uri.t -> result

  val saveable_uri_field 
    : string -> old:Uri.t option -> new':Uri.t option -> result

  val string_field : string -> old:string -> new':string -> result

  val string_list_field 
    : string -> old:string list -> new':string list -> result

  val string_pair_list_field 
     : string -> old:(string * string) list -> new':(string * string) list 
    -> result

  val source_field
    : string -> old:Rdb_dest.Source.t -> new':Rdb_dest.Source.t -> result

  val source_list_field
     : string -> old:Rdb_dest.Source.t list -> new':Rdb_dest.Source.t list 
    -> result

  val optional
     : (string -> old:'value -> new':'value -> result)
    -> string 
    -> old:'value option
    -> new':'value option
    -> result
end