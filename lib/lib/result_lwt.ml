
open Base


type 'ok t = ('ok, exn) Result.t Lwt.t


let return a = Lwt.return (Ok a)
let fail exn = Lwt.return (Error exn)

let try_return f = try return @@ f () with exn -> fail exn

let return_lwt a = Lwt.try_bind (fun () -> a) return fail
let fail_lwt exn = Lwt.try_bind (fun () -> exn) fail fail


let catch_lwt_exn a =
  Lwt.try_bind
    (fun () -> a)
    (fun a -> Lwt.return a)
    (fun exn -> fail exn)


let bind a ~f =
  Lwt.bind a begin function
    | Error e -> fail e
    | Ok v -> f v
  end
  |> catch_lwt_exn


let map a ~f =
  a |> Lwt.map (Result.map ~f) |> catch_lwt_exn


let both a b = 
  Lwt.bind a (fun a -> Lwt.map (Result.Let_syntax.Let_syntax.both a) b)
  |> catch_lwt_exn


let to_lwt t =
  match%lwt t with
  | Error exn -> Lwt.fail exn
  | Ok v -> Lwt.return v
  

module Let_syntax = struct
  let return = return
  let bind = bind
  let map = map
  let both = both

  module Open_on_rhs = struct
    let return = return
    let fail = fail
    let try_return = try_return
    let return_lwt = return_lwt
    let fail_lwt = fail_lwt
  end
end


module Open_on_rhs = Let_syntax.Open_on_rhs


module Just_let_syntax = struct
  module Let_syntax = Let_syntax
end