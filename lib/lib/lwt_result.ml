
open Base


type ('ok, 'bad) t = ('ok, 'bad) Result.t Lwt.t


let return a = a |> Result.return |> Lwt.return
let fail a = a |> Result.fail |> Lwt.return

let try_return f = try return @@ f () with exn -> fail exn

let return_lwt a = Lwt.map Result.return a
let fail_lwt a = Lwt.map Result.fail a


module Let_syntax = struct
  let return = return

  let bind a  ~f =
    Lwt.bind a begin function
      | Error e -> fail e
      | Ok v -> f v
    end

  let map a ~f = Lwt.map (Result.map ~f) a

  let both a b = 
    Lwt.bind a (fun a -> Lwt.map (Result.Let_syntax.Let_syntax.both a) b)

  module Open_on_rhs = struct
    let return = return
    let fail = fail
    let try_return = try_return
    let return_lwt = return_lwt
    let fail_lwt = fail_lwt
  end
end


module Just_let_syntax = struct
  module Let_syntax = Let_syntax
end