

module File_store = struct
  module Max_usage = struct
    type t = int option

    let valid_pattern = "^([1-9]\\d*)([kmgt])?$" |> Re.Perl.compile_pat

    let clean s = 
      let separators = "[_, ]+" |> Re.Perl.compile_pat in
      s 
      |> Re.replace_string separators ~by:""
      |> String.lowercase_ascii

    let validate s =
      match s with
      | None -> true
      | Some s -> 
        s |> clean |> Re.execp valid_pattern

    let wrap s = 
      if not (validate s) then failwith "Invalid byte string."
      else

      match s with
      | None -> 
          None

      | Some s -> begin
        let matches = s |> clean |> Re.exec valid_pattern in
        match matches |> Re.Group.all with 
        | [| _; amount; "" |] -> 
            Some (amount |> int_of_string)

        | [| _; amount; suffix |] -> begin
          let amount = amount |> int_of_string in
          match suffix with 
          | "k" -> Some (amount * 1024)
          | "m" -> Some (amount * 1024 * 1024)
          | "g" -> Some (amount * 1024 * 1024 * 1024)
          | "t" -> Some (amount * 1024 * 1024 * 1024 * 1024)
          | suffix -> 
              Printf.sprintf "Unknown suffix '%s'" suffix 
              |> failwith
        end

        | _ -> 
          failwith "Unexpected array structure returned from Re.Group.all"
      end
    
    let unwrap t = 
      match t with
      | None ->
        None

      | Some b ->
        Some (b |> string_of_int)

  end
end