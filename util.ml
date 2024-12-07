open Core

let time f x =
  let start = Time_float.now () in
  let fx = f x in
  let elapsed = Time_float.diff (Time_float.now ()) start in
  Printf.printf "Execution time: %s\n" (Time_float.Span.to_string_hum elapsed);
  fx
