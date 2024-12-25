open Core

type code = Lock of int list | Key of int list

let parse_code field =
  let histogram =
    Array.transpose_exn field |> Array.to_list
    |> List.map ~f:(Array.count ~f:(Char.equal '#'))
  in
  match field.(0).(0) with
  | '#' -> Lock histogram
  | '.' -> Key histogram
  | _ -> failwith "unknown shit"

let read_locks fname =
  let all =
    In_channel.read_all fname
    |> Re.split (Re.compile (Re.str "\n\n"))
    |> List.map ~f:Myfield.of_string
    |> List.map ~f:parse_code
  in
  let locks =
    List.filter_map all ~f:(function Lock l -> Some l | Key _ -> None)
  in
  let keys =
    List.filter_map all ~f:(function Lock _ -> None | Key k -> Some k)
  in
  (locks, keys)

let solve1 fname =
  let locks, keys = read_locks fname in
  Sequence.cartesian_product (Sequence.of_list locks) (Sequence.of_list keys)
  |> Sequence.count ~f:(fun (l, k) ->
         List.zip_exn l k
         |> List.find ~f:(fun (lv, kv) -> lv + kv > 7)
         |> is_none)

let () =
  assert (solve1 "test/day25.txt" = 3);
  assert (solve1 "data/day25.txt" = 3327)
