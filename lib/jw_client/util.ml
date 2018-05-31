
open Lwt.Infix


(** [merge_params la lb] merges the parameters of [lb] into [la], replacing
    the values of any keys that already exists. *)
let merge_params la lb = 
  let la' = la |> List.filter (fun (ka, _) ->
    let exists = lb |> List.exists (fun (kb, _) -> kb == ka) in
    not exists
  ) in

  List.fold_left (fun l p -> p :: l) lb la'