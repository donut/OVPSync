
(** [video Log ~check_md5 old new'] Checks whether or not there are  
    significnat differenes between [old] and [knew], returning [true] if there  
    are. If [debug] is [true], then it will print out the status of each 
    field checked. [check_md5] determines whether or not the [md5] field is 
    checked.*)
val video 
   : (module Logger.Sig) 
  -> check_md5:bool
  -> Rdb_dest.Video.t
  -> Rdb_dest.Video.t
  -> bool Lwt.t