
open Printf
open Lib.Infix.Option


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
    |  `Warn -> (30,  "warn", "☢️")
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
  val level : Level.t

  val log : Level.t -> ?exn:exn -> string -> unit Lwt.t
  val logf : Level.t -> ?exn:exn -> ('a, unit, string, unit Lwt.t) format4 -> 'a
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


let last_entry = ref None


module type Config = sig
  val prefix : string
  val level : Level.t
end


module Make (Conf : Config) : Sig = struct
  let level = Conf.level


  let date_string_of_timestamp ts =
    let { Unix.tm_year=yr; tm_mon=mon; tm_mday=day; _ } = Unix.localtime ts in
    sprintf "%4d/%02d/%02d" (yr + 1900) (mon + 1) day


  let time_string_of_timestamp ts =
    let { Unix. tm_hour=hr; tm_min=min; tm_sec=sec; _ } = Unix.localtime ts in
    sprintf "%2d:%02d:%02d" hr min sec


  let maybe_print_full_date now last =
    let { Unix.tm_yday=now_day; _ } = Unix.localtime now in
    let { Unix.tm_yday=then_day; _ } = Unix.localtime (last =?: 0.) in

    if last = None || now_day <> then_day then
      let date = date_string_of_timestamp now in
      let separator = "##################################################" in
      Lwt_io.printlf
        "%s\n                    %s                    \n%s"
        separator date separator
    else
      Lwt.return ()


  let log (level : Level.t) ?exn message =
    if Level.compare Conf.level level < 0 then Lwt.return ()
    else
    let now = Unix.time () in

    let%lwt () = maybe_print_full_date now !last_entry in
    let () = last_entry := Some now in

    let time = time_string_of_timestamp now in
    let lvl = Level.to_symbol level in

    let message = 
      match exn with
      | None -> message
      | Some e -> message ^ " Exception: " ^ (Printexc.to_string e)
    in

    Lwt_io.printlf "%s %s %s  %s" time Conf.prefix lvl message


  let logf (level : Level.t) ?exn fmt =
    ksprintf (log level ?exn)  fmt


  let fatal ?exn message =
    log `Fatal ?exn message
  let fatalf ?exn fmt =
    ksprintf (fatal ?exn) fmt
  let error ?exn message =
    log `Error ?exn message
  let errorf ?exn fmt =
    ksprintf (error ?exn) fmt
  let warn ?exn message =
    log `Warn ?exn message
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