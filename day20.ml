open Core

let calc_distances field ~start =
  let cmp_dist (_pos1, dist1) (_pos2, dist2) = compare dist1 dist2 in
  let queue = Pairing_heap.of_list ~cmp:cmp_dist [ (start, 0) ] in
  let visited = Hashtbl.create (module Myfield.Point) in
  let distances = Hashtbl.create (module Myfield.Point) in
  let rec dijkstra () =
    match Pairing_heap.pop queue with
    | None -> distances
    | Some (pos, _dist) when Hashtbl.mem visited pos -> dijkstra ()
    | Some (pos, dist) ->
        Hashtbl.set visited ~key:pos ~data:();

        let perspective_neighbors =
          Myfield.Point.neighbors pos
          |> List.filter ~f:(Myfield.contains field)
          |> List.filter ~f:(fun n -> not (Myfield.eq_at ~field ~c:'#' n))
          |> List.filter ~f:(fun n ->
                 match Hashtbl.find distances n with
                 | None -> true
                 | Some d -> dist + 1 < d)
        in

        List.iter perspective_neighbors ~f:(fun n ->
            Hashtbl.set distances ~key:n ~data:(dist + 1);
            Pairing_heap.add queue (n, dist + 1));

        dijkstra ()
  in
  dijkstra ()

type state = {
  pos : Myfield.Point.t;
  cheat : Myfield.Point.t option;
  cheat_stop : Myfield.Point.t option;
  cheat_duration : int;
  steps : int;
  visited : Myfield.PointSet.t;
}
[@@deriving sexp]

type cheat = { start : Myfield.Point.t; stop : Myfield.Point.t }
[@@deriving sexp, compare, equal]

let find_ways field ~distances ~from ~fair_best ~cheat_power =
  let res = Hashtbl.Poly.create () in
  let rec bfs queue =
    match queue with
    | [] -> res
    (*
       Nah, too long
    *)
    | { steps; _ } :: tail when steps >= fair_best -> bfs tail
    (*
       Woops. Crashed
    *)
    | { pos; cheat_duration = 0; _ } :: tail
      when Myfield.eq_at ~field ~c:'#' pos ->
        bfs tail
    (*
       Reached finish with active cheat. Good job!
    *)
    | { pos; steps; cheat = Some cheat_start; _ } :: tail
      when Myfield.eq_at ~field ~c:'E' pos ->
        Hashtbl.add_exn res ~key:{ start = cheat_start; stop = pos } ~data:steps;
        bfs tail
    (*
       Cheat ended. Calculate the distance to the finish
    *)
    | ({ cheat = Some start; cheat_stop = Some stop; _ } as state) :: tail ->
        let finish_distance =
          Hashtbl.find_exn distances state.pos + state.steps
        in
        if finish_distance < fair_best then
          Hashtbl.add_exn res ~key:{ start; stop } ~data:finish_distance;
        bfs tail
    (*
       Looking for a place to cheat
    *)
    | state :: tail ->
        let neighbors =
          Myfield.Point.neighbors state.pos
          |> List.filter ~f:(Myfield.contains field)
          |> List.filter ~f:(fun pos -> not (Set.mem state.visited pos))
        in

        (* Free way could just land here *)
        let free_steps =
          neighbors
          |> List.filter ~f:(Fun.negate (Myfield.eq_at ~field ~c:'#'))
          |> List.map ~f:(fun pos -> { state with pos })
        in

        (* If we still can activate or use a cheat -- step on the walls as well *)
        let walls = neighbors |> List.filter ~f:(Myfield.eq_at ~field ~c:'#') in
        let cheat_steps =
          match state.cheat with
          | None ->
              walls
              |> List.map ~f:(fun pos ->
                     {
                       state with
                       pos;
                       cheat = Some pos;
                       cheat_duration = cheat_power;
                     })
          | Some _ when state.cheat_duration > 0 ->
              walls |> List.map ~f:(fun pos -> { state with pos })
          | _ ->
              failwith
                (sprintf "unreachable: %s"
                   (sexp_of_state state |> Sexp.to_string_hum))
        in

        (* Increase steps, decrease cheat active time *)
        let next =
          free_steps @ cheat_steps
          |> List.map ~f:(fun state ->
                 {
                   state with
                   steps = state.steps + 1;
                   visited = Set.add state.visited state.pos;
                   cheat_duration = state.cheat_duration - 1;
                   cheat_stop =
                     (if state.cheat_duration = 1 then Some state.pos else None);
                 })
        in

        bfs (tail @ next)
  in
  bfs
    [
      {
        pos = from;
        cheat = None;
        cheat_stop = None;
        cheat_duration = 0;
        steps = 0;
        visited = Myfield.PointSet.of_list [ from ];
      };
    ]

let solve1 fname =
  let field = Myfield.read fname in
  let start = Myfield.find field ~f:(Char.equal 'S') |> List.hd_exn in
  let finish = Myfield.find field ~f:(Char.equal 'E') |> List.hd_exn in
  let distances = calc_distances field ~start:finish in
  let fair_best = Hashtbl.find_exn distances start in

  find_ways field ~distances ~from:start ~fair_best ~cheat_power:2
  |> Hashtbl.Poly.to_alist
  |> List.map ~f:(fun (_cheat, dist) -> dist)
  |> List.count ~f:(fun d -> fair_best - d >= 100)

let () = assert (solve1 "data/day20.txt" = 1454)
