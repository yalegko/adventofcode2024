open Core

let group_by_alist alist =
  alist |> Char.Table.of_alist_multi |> Hashtbl.to_alist

let list_antenas field =
  Myfield.find field ~f:(Fn.non (Char.equal '.'))
  |> List.map ~f:(fun (x, y) -> (field.(x).(y), (x, y)))
  |> group_by_alist

let combinations list =
  let rec loop list res =
    match list with
    | [] -> res
    | x :: tail ->
        loop tail (List.append res (List.map tail ~f:(fun y -> (x, y))))
  in
  loop list []

let list_antinodes field antenas =
  combinations antenas
  (* `a` should be a midpoint of line #-a-a, so it's corrds will be x1=(x+x2)/2, y1=(y+y2)/2 *)
  |> List.concat_map ~f:(fun ((x1, y1), (x2, y2)) ->
         [ ((2 * x1) - x2, (2 * y1) - y2); ((2 * x2) - x1, (2 * y2) - y1) ])
  |> List.filter ~f:(Myfield.contains field)

let solve1 fname =
  let field = Myfield.read fname in
  list_antenas field
  |> List.concat_map ~f:(fun (_freq, points) -> list_antinodes field points)
  |> List.dedup_and_sort ~compare:Myfield.Point.compare
  |> List.length

let () =
  assert (solve1 "test/day08.txt" = 14);
  assert (solve1 "data/day08.txt" = 423)

let next_points p1 p2 =
  let next (x1, y1) (x2, y2) =
    let dx = x2 - x1 and dy = y2 - y1 in
    (x2 + dx, y2 + dy)
  in
  Sequence.unfold ~init:(p1, p2) ~f:(fun (p1, p2) ->
      let new_p = next p1 p2 in
      Some (new_p, (p2, new_p)))

let line_points (p1, p2) ~filter =
  [
    Sequence.of_list [ p1; p2 ];
    Sequence.take_while ~f:filter (next_points p1 p2);
    Sequence.take_while ~f:filter (next_points p2 p1);
  ]
  |> List.reduce_exn ~f:Sequence.append

let list_antinodes2 field antenas =
  combinations antenas
  |> List.map ~f:(line_points ~filter:(Myfield.contains field))
  |> List.concat_map ~f:Sequence.to_list

let solve2 fname =
  let field = Myfield.read fname in
  list_antenas field
  |> List.concat_map ~f:(fun (_freq, points) -> list_antinodes2 field points)
  |> List.dedup_and_sort ~compare:Myfield.Point.compare
  |> List.length

let () =
  assert (solve2 "test/day08.txt" = 34);
  assert (solve2 "data/day08.txt" = 1287)
