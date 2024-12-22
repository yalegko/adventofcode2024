open Core

let time f x =
  let start = Time_float.now () in
  let fx = f x in
  let elapsed = Time_float.diff (Time_float.now ()) start in
  print_endline
    (sprintf "Execution time: %s" (Time_float.Span.to_string_hum elapsed));
  fx
