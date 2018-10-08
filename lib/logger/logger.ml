
open Printf

module Level = struct
  type t = 
    [ `Off
    | `Fatal
    | `Error
    | `Warn
    | `Info
    | `Debug
    | `Trace ]

  let to_tup = function
    |   `Off -> ( 0,   "off", "🔳")
    | `Fatal -> (10, "fatal", "🛑")
    | `Error -> (20, "error", "❌")
    |  `Warn -> (30,  "warn", "⚠️")
    |  `Info -> (40,  "info", "ℹ️")
    | `Debug -> (50, "debug", "♒️")
    | `Trace -> (60, "trace", "🔎")

  let wrap_for = function
    |   `Off -> "␀", "␀"
    | `Fatal -> "📛", "📛"
    | `Error -> "❗️", "❗"
    |  `Warn -> "⚠️", "⚠️"
    |  `Info -> "•", "•"
    | `Debug -> "‹", "›"
    | `Trace -> "·", "·"

  let to_string t =
    t |> to_tup |> BatTuple.Tuple3.second
  let to_int t =
    t |> to_tup |> BatTuple.Tuple3.first
  let to_symbol t = 
    t |> to_tup |> BatTuple.Tuple3.third
  let compare a b =
    BatInt.compare (to_int a) (to_int b)
end

module type Sig = sig
  val log : Level.t -> string -> unit Lwt.t
  val logf : Level.t -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  val fatal : ?exn:exn -> string -> unit Lwt.t
  val fatalf : ?exn:exn -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  val error : ?exn:exn -> string -> unit Lwt.t
  val errorf : ?exn:exn -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  val warn : ?exn:exn -> string -> unit Lwt.t
  val warnf : ?exn:exn -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  val info : string -> unit Lwt.t
  val infof : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val debug : string -> unit Lwt.t
  val debugf : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val trace : string -> unit Lwt.t
  val tracef : ('a, unit, string, unit Lwt.t) format4 -> 'a
end

module type Config = sig
  val prefix : string
  val level : Level.t
end

module Make (Conf : Config) : Sig = struct
  let datetime () =
    let { Unix.tm_year=yr; tm_mon=mon; tm_mday=day;
          tm_hour=hr; tm_min=min; tm_sec=sec; _ }
      = Unix.time () |> Unix.localtime
    in
    sprintf "%4d/%02d/%02d %2d:%02d:%02d" (yr + 1900) (mon + 1) day hr min sec

  let log (level : Level.t) message =
    if Level.compare Conf.level level < 0 then
      Lwt.return ()
    else
      let datetime = datetime () in
      let lvl = Level.to_symbol level in
      Lwt_io.printlf "%s %s %s  %s" datetime Conf.prefix lvl message

  let logf (level : Level.t) fmt =
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