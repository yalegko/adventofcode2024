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

type state = { score : int; direction : char; path : (int * int) list }

let do_step state =
  let pos = List.hd_exn state.path in
  let next = Myfield.do_step ~direction:state.direction pos in
  { state with path = next :: state.path; score = state.score + 1 }

let turn state turn =
  let turn_cost = 1000 in
  {
    state with
    direction = turn state.direction;
    score = state.score + turn_cost;
  }

let best_way field ~from =
  let best_paths = ref [] in
  let best_score = ref 100500 in
  let visited = Hashtbl.create (module Myfield.PointWithDir) in
  let rec bfs queue =
    match queue with
    | [] -> (!best_score, !best_paths)
    | state :: tail -> (
        let ((x, y) as pos) = List.hd_exn state.path in
        match field.(x).(y) with
        | '#' ->
            (* Wall *)
            bfs tail
        | 'E' when state.score < !best_score ->
            (* New best *)
            best_score := state.score;
            best_paths := [ state.path ];
            bfs tail
        | 'E' when state.score = !best_score ->
            (* Current best *)
            best_paths := state.path :: !best_paths;
            bfs tail
        | _ when state.score >= !best_score ->
            (* Went too deep *)
            bfs tail
        | _ -> (
            (* Keep going *)
            match Hashtbl.find visited (pos, state.direction) with
            | Some old_score when old_score < state.score -> bfs tail
            | _ ->
                Hashtbl.set visited ~key:(pos, state.direction)
                  ~data:state.score;
                bfs
                  (List.append tail
                     [ do_step state; turn state turn_cw; turn state turn_ccw ])
            ))
  in
  bfs [ { score = 0; path = [ from ]; direction = '>' } ]

let solve1 fname =
  let field = Myfield.read fname in
  let start = Myfield.find field ~f:(Char.equal 'S') |> List.hd_exn in
  best_way field ~from:start |> fst

let () =
  (* assert (Util.time solve1 "data/day16.txt" = 83432); *)
  assert (solve1 "test/day16.txt" = 7036);
  assert (solve1 "test/day16-2.txt" = 11048)

let solve2 fname =
  let field = Myfield.read fname in
  let start = Myfield.find field ~f:(Char.equal 'S') |> List.hd_exn in

  let best_paths = best_way field ~from:start |> snd in
  List.concat best_paths
  |> List.dedup_and_sort ~compare:Myfield.Point.compare
  |> List.length

let () =
  (* assert (Util.time solve2 "data/day16.txt" = 467); *)
  assert (solve2 "test/day16.txt" = 45);
  assert (solve2 "test/day16-2.txt" = 64)
