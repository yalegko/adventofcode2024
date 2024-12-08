open Core

module Point = struct
  type t = int * int [@@deriving compare, sexp]

  let to_string p = p |> sexp_of_t |> Sexp.to_string_hum
end

let read_field file =
  In_channel.read_lines file |> List.map ~f:String.to_array |> List.to_array

let get field (i, j) = try Some field.(i).(j) with Invalid_argument _ -> None

let list_antenas field =
  Array.mapi field ~f:(fun i row ->
      Array.filter_mapi row ~f:(fun j elem ->
          if not (Char.equal elem '.') then Some (elem, (i, j)) else None))
  |> Array.map ~f:List.of_array |> List.of_array |> List.concat

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
  |> List.map ~f:(fun ((x1, y1), (x2, y2)) ->
         [ ((2 * x1) - x2, (2 * y1) - y2); ((2 * x2) - x1, (2 * y2) - y1) ])
  |> List.concat
  |> List.filter ~f:(fun pos -> get field pos |> is_some)

let solve1 fname =
  let field = read_field fname in
  let antenas_map = list_antenas field |> Char.Table.of_alist_multi in
  Hashtbl.to_alist antenas_map
  |> List.map ~f:(fun (_freq, points) -> list_antinodes field points)
  |> List.concat
  |> List.dedup_and_sort ~compare:Point.compare
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
  let in_field p = get field p |> is_some in
  combinations antenas
  |> List.map ~f:(line_points ~filter:in_field)
  |> List.map ~f:Sequence.to_list
  |> List.concat

let group_by_alist alist =
  alist |> Char.Table.of_alist_multi |> Hashtbl.to_alist

let solve2 fname =
  let field = read_field fname in
  list_antenas field |> group_by_alist
  |> List.map ~f:(fun (_freq, points) -> list_antinodes2 field points)
  |> List.concat
  |> List.dedup_and_sort ~compare:Point.compare
  |> List.length

let () =
  assert (solve2 "test/day08.txt" = 34);
  assert (solve2 "data/day08.txt" = 1287)
