
open Lwt.Infix

let (>>=?) m f =
  m >>= function | Ok x -> f x | Error err -> Lwt.return (Error err)

let report_error = function  
  | Ok () -> Lwt.return_unit
  | Error err ->
    Lwt_io.eprintl (Caqti_error.show err)

let connect = Caqti_lwt.connect 