
let make_jw_src
  db_pool log_level (client : Config.Jw_client.t) (src : Config.Jw_source.t)
= 
  let module Client_log = Logger.Make(struct
    let prefix = client.log_namespace
    let level = log_level client.log_level
  end) in

  let module Client = Jw_client.Platform.Make(Client_log)(struct 
    let key = client.key
    let secret = client.secret
    let rate_limit_to_leave = client.rate_limit_to_leave
  end) in

  let module Var_store = Variable_store.Make(struct
    let db_pool = db_pool
    let namespace = "JWsrc-" ^ client.key
  end) in

  let module Src_log = Logger.Make(struct
    let prefix = src.log_namespace
    let level = log_level src.log_level
  end) in

  let module Src = Jw_source.Make(Client)(Var_store)(Src_log)(struct
    let chunk_size = src.chunk_size |> string_of_int
    
    let params = ["result_limit", [chunk_size]] 
    let temp_pub_tag = src.temp_publish_tag

    let backup_expires_field = src.backup_expires_field
  end) in

  (module Src : Jw_source.Made)


let make_rdb_dest db_pool log_level (conf : Config.Rdb_dest.t) =
  let module Log = Logger.Make(struct
    let prefix = conf.log_namespace
    let level = log_level conf.log_level
  end) in

  let module Dest = Rdb_dest.Make(Log)(struct
    let db_pool = db_pool
    let files_path = conf.files_path
  end) in

  (module Dest : Rdb_dest.Made)