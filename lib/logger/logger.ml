
open Printf

module type Config = sig
  val prefix : string
  val level : Sync.logger_level
end

module Level = struct
  type t = Sync.logger_level

  let to_tup = function
    | `Off   -> ( 0,   "off")
    | `Fatal -> (10, "fatal")
    | `Error -> (20, "error")
    | `Warn  -> (30,  "warn")
    | `Info  -> (40,  "info")
    | `Debug -> (50, "debug")
    | `Trace -> (60, "trace")

  let wrap_for = function
    | `Fatal -> "!!!", "!!!"
    | `Error -> "!#!", "!#!"
    |  `Warn ->  "#!", "#!"
    |      _ ->   "[", "]"

  let to_string t =
    t |> to_tup |> snd
  let to_int t =
    t |> to_tup |> fst
  let compare a b =
    BatInt.compare (to_int a) (to_int b)
end

module Make (Conf : Config) = struct
  let datetime () =
    let { Unix.tm_year=yr; tm_mon=mon; tm_mday=day;
          tm_hour=hr; tm_min=min; tm_sec=sec }
        = Unix.time () |> Unix.localtime
    in
    sprintf "%4d/%02d/%02d %2d:%02d:%02d" (yr + 1900) (mon + 1) day hr min sec

  let log (level:Sync.logger_level) message =
    if Level.compare Conf.level level < 0 then
      Lwt.return ()
    else
      let datetime = datetime () in
      let lvl = Level.to_string level in
      let lw, rw = Level.wrap_for level in
      Lwt_io.printlf "%s %s%s%s %s: %s" datetime lw lvl rw Conf.prefix message

  let logf (level:Sync.logger_level) fmt =
    ksprintf (log level) fmt

  let add_exn_to_message exn message =
    message ^ match exn with
      | None -> ""
      | Some e -> " Exception: " ^ (Printexc.to_string e)

  let fatal ?exn message =
    log `Fatal (add_exn_to_message exn message)
  let fatalf ?exn fmt =
    ksprintf (fatal ?exn) fmt
  let error ?exn message =
    log `Error (add_exn_to_message exn message)
  let errorf ?exn fmt =
    ksprintf (error ?exn) fmt
  let warn ?exn message =
    log `Warn (add_exn_to_message exn message)
  let warnf ?exn fmt =
    ksprintf (warn ?exn) fmt
  let info message =
    log `Info message
  let infof fmt =
    ksprintf info fmt
  let debug message =
    log `Debug message
  let debugf fmt =
    ksprintf debug fmt
  let trace message =
    log `Trace message
  let tracef fmt =
    ksprintf trace fmt

end