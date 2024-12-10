open Core

let read_map fname =
  Myfield.read fname
  |> Array.map ~f:(Array.map ~f:(fun c -> int_of_char c - int_of_char '0'))

let can_go field (x, y) =
  let cur_height = field.(x).(y) in
  [ (x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1) ]
  |> List.filter ~f:(Myfield.contains field)
  |> List.filter ~f:(fun (x1, y1) -> field.(x1).(y1) = cur_height + 1)

module VisitedSet = Set.Make (Myfield.Point)

let number_of_ways_up field start =
  let rec bfs res queue =
    match queue with
    | [] -> Set.length res
    | (x, y) :: tail when field.(x).(y) = 9 -> bfs (Set.add res (x, y)) tail
    | p :: tail -> bfs res (tail @ can_go field p)
  in
  bfs VisitedSet.empty [ start ]

let solve1 fname =
  let map = read_map fname in
  map
  |> Myfield.find ~f:(Int.equal 0)
  |> List.map ~f:(number_of_ways_up map)
  |> List.reduce_exn ~f:( + )

let () =
  assert (solve1 "test/day10.txt" = 36);
  assert (solve1 "data/day10.txt" = 820)

module Path = struct
  type t = (int * int) list [@@deriving compare, sexp]
end

module PathSet = Set.Make (Path)

let ways_up field start =
  let rec bfs res queue =
    match queue with
    | [] -> res
    | ((x, y) :: _ as path) :: tail when field.(x).(y) = 9 ->
        bfs (Set.add res (List.rev path)) tail
    | (p :: _ as path) :: tail ->
        let new_paths =
          can_go field p |> List.map ~f:(fun next -> next :: path)
        in
        bfs res (tail @ new_paths)
    | _ -> failwith "unreachable"
  in
  bfs PathSet.empty [ [ start ] ]

let solve2 fname =
  let map = read_map fname in
  map
  |> Myfield.find ~f:(Int.equal 0)
  |> List.map ~f:(ways_up map)
  |> List.map ~f:Set.length |> List.reduce_exn ~f:( + )

let () =
  assert (solve2 "test/day10.txt" = 81);
  assert (solve2 "data/day10.txt" = 1786)
