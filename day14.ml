open Core

type robot = { pos : int * int; v : int * int }

let parse_robot line =
  let robot =
    line
    |> Re.all (Re.compile (Re.Perl.re {|-?\d+|}))
    |> List.map ~f:Re.Group.all
    |> List.map ~f:(fun d -> int_of_string d.(0))
  in
  match robot with
  | [ py; px; vy; vx ] -> { pos = (px, py); v = (vx, vy) }
  | _ -> failwith "Invalid robot input format"

let read_robots fname = In_channel.read_lines fname |> List.map ~f:parse_robot

(*  mod' returns always positive reminder *)
let mod' a n = if a < 0 then ((a mod n) + n) mod n else a mod n

let move robot ~bounds:(n, m) ~time:t =
  let { pos = px, py; v = vx, vy } = robot in
  let newx = mod' (px + (vx * t)) n in
  let newy = mod' (py + (vy * t)) m in
  { robot with pos = (newx, newy) }

let () =
  let r = move (parse_robot "p=2,4 v=2,-3") ~bounds:(7, 11) ~time:5 in
  assert (Tuple2.equal ~eq1:( = ) ~eq2:( = ) r.pos (3, 1))

let solve1 fname bounds =
  let final =
    read_robots fname
    |> List.map ~f:(move ~bounds ~time:100)
    |> List.map ~f:(fun r -> r.pos)
  in
  let n, m = bounds in
  let q1 (x, y) = x < n / 2 && y < m / 2
  and q2 (x, y) = x < n / 2 && y > m / 2
  and q3 (x, y) = x > n / 2 && y > m / 2
  and q4 (x, y) = x > n / 2 && y < m / 2 in
  [ q1; q2; q3; q4 ]
  |> List.map ~f:(fun belongs_to_q -> List.count final ~f:belongs_to_q)
  |> List.reduce_exn ~f:( * )

let () =
  assert (solve1 "test/day14.txt" (7, 11) = 12);
  assert (solve1 "data/day14.txt" (103, 101) = 228457125)

let print_robots robots ~bounds:(n, m) =
  let field = Array.make_matrix ~dimx:n ~dimy:m '.' in
  let print field =
    Array.iter field ~f:(fun row -> print_endline (String.of_array row))
  in
  List.iter robots ~f:(fun r -> field.(fst r.pos).(snd r.pos) <- '#');
  print field

let forms_frame robots =
  let positions = Hashtbl.create (module Myfield.Point) in
  List.iter robots ~f:(fun r -> Hashtbl.set positions ~key:r.pos ~data:());
  let is_robot r = Hashtbl.mem positions r in

  let rec right_edge (x, y) =
    match (x, y + 1) with
    | next when is_robot next -> right_edge next
    | _ -> (x, y)
  in
  let rec bottom_edge (x, y) =
    match (x + 1, y) with
    | next when is_robot next -> bottom_edge next
    | _ -> (x, y)
  in
  Hashtbl.keys positions
  |> List.find ~f:(fun ((x, y) as p) ->
         (* Assuming that we go from top left to bottom right corner of the frame:
             - we can go `right->down` or `down->right` and it will be same point
             - plus filter out too small frames (e.g. only 5 cells) *)
         let maxx, maxy = bottom_edge (right_edge p) in
         maxx - x > 5
         && maxy - y > 5
         && Myfield.Point.equal (maxx, maxy) (right_edge (bottom_edge p)))
  |> is_some

let solve2 fname bounds =
  let robots = read_robots fname in
  let i, _final =
    Sequence.unfold ~init:1 ~f:(fun s -> Some (s, s + 1))
    |> Sequence.map ~f:(fun i -> (i, List.map robots ~f:(move ~bounds ~time:i)))
    |> Sequence.find_exn ~f:(fun (_i, robots) -> forms_frame robots)
  in
  (* print_robots final ~bounds; *)
  i

(* let () = assert (Util.time solve2 "data/day14.txt" (103, 101) = 6493) *)
