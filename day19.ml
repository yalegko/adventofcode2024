open Core

let read_task fname =
  let towels, patterns =
    In_channel.read_all fname |> Re.split (Re.compile (Re.str "\n\n"))
    |> function
    | [ f; m ] -> (f, m)
    | _ -> failwith "Unexpected input"
  in
  let towels = Re.split (Re.compile (Re.str ", ")) towels in
  let patterns = String.split_lines patterns in
  (towels, patterns)

let rec is_possible ~towels pattern =
  match pattern with
  | "" -> true
  | s ->
      towels
      |> List.filter_map ~f:(fun towel -> String.chop_prefix s ~prefix:towel)
      |> List.exists ~f:(is_possible ~towels)

let solve1 fname =
  let towels, patters = read_task fname in
  List.filter patters ~f:(is_possible ~towels) |> List.length

let () =
  assert (solve1 "test/day19.txt" = 6);
  assert (solve1 "data/day19.txt" = 238)

let ways_to_combine ~towels pattern =
  let cache = String.Table.create () in
  let rec count_ways pattern =
    let res =
      match pattern with
      | "" -> 1
      | s when Hashtbl.mem cache s -> Hashtbl.find_exn cache s
      | s ->
          towels
          |> List.filter_map ~f:(fun t -> String.chop_prefix s ~prefix:t)
          |> List.map ~f:count_ways |> List.fold ~init:0 ~f:( + )
    in
    Hashtbl.set cache ~key:pattern ~data:res;
    res
  in
  count_ways pattern

let solve2 fname =
  let towels, patterns = read_task fname in
  patterns |> List.map ~f:(ways_to_combine ~towels) |> List.reduce_exn ~f:( + )

let () =
  assert (solve2 "test/day19.txt" = 16);
  assert (solve2 "data/day19.txt" = 635018909726691)
