open Core

let turn_cw d =
  match d with
  | '>' -> 'v'
  | 'v' -> '<'
  | '<' -> '^'
  | '^' -> '>'
  | _ -> failwithf "impossible direction %c" d ()

let turn_ccw d = d |> turn_cw |> turn_cw |> turn_cw
let () = assert (Char.equal (turn_ccw '>') '^')

let best_way field ~from ~to' =
  let turn_cost = 1000 in
  let visited = Hashtbl.create (module Myfield.PointWithDir) in
  let best_paths = ref [] in
  let best_score =
    ref (turn_cost * Array.length field * Array.length field.(0))
  in
  let rec bfs queue =
    match queue with
    | [] -> (!best_score, !best_paths)
    (* Finish *)
    | ((pos :: _ as path), _direction, score) :: tail
      when Myfield.Point.equal pos to' ->
        if score < !best_score then (
          best_score := score;
          best_paths := [ path ])
        else if score = !best_score then best_paths := path :: !best_paths;
        bfs tail
    (* Wall *)
    | ((x, y) :: _, _, _) :: tail when Char.equal field.(x).(y) '#' -> bfs tail
    (* Keep going *)
    | ((pos :: _ as path), direction, score) :: tail -> (
        match Hashtbl.find visited (pos, direction) with
        | Some old_score when old_score < score -> bfs tail
        | _ ->
            Hashtbl.set visited ~key:(pos, direction) ~data:score;
            bfs
              (List.append tail
                 [
                   (Myfield.do_step ~direction pos :: path, direction, score + 1);
                   (path, turn_cw direction, score + turn_cost);
                   (path, turn_ccw direction, score + turn_cost);
                 ]))
    | ([], _, _) :: _tail -> failwith "unreachable"
  in
  bfs [ ([ from ], '>', 0) ]

let solve1 fname =
  let field = Myfield.read fname in
  let start = Myfield.find field ~f:(Char.equal 'S') |> List.hd_exn in
  let finish = Myfield.find field ~f:(Char.equal 'E') |> List.hd_exn in
  best_way field ~from:start ~to':finish |> fst

let () =
  (* assert (Util.time solve1 "data/day16.txt" = 83432); *)
  assert (solve1 "test/day16.txt" = 7036);
  assert (solve1 "test/day16-2.txt" = 11048)

let solve2 fname =
  let field = Myfield.read fname in
  let start = Myfield.find field ~f:(Char.equal 'S') |> List.hd_exn in
  let finish = Myfield.find field ~f:(Char.equal 'E') |> List.hd_exn in

  let best_paths = best_way field ~from:start ~to':finish |> snd in
  List.concat best_paths
  |> List.dedup_and_sort ~compare:Myfield.Point.compare
  |> List.length

let () =
  (* assert (Util.time solve2 "data/day16.txt" = 467); *)
  assert (solve2 "test/day16.txt" = 45);
  assert (solve2 "test/day16-2.txt" = 64)
