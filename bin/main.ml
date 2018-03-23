
let () = 
  let conf = Lib.Conf.(JW_source.of_string @@ read_file "config.json") in
  print_endline "Good morning, Starshine. The Earth says, \"Hello!\"";
  Printf.printf "JW API key: %s" @@ Lib.Conf.JW_source.key conf