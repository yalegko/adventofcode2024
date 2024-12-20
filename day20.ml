open Core

type state = {
  steps : int;
  pos : Myfield.Point.t;
  visited : Myfield.PointSet.t;
}

type cheat = { start : Myfield.Point.t; stop : Myfield.Point.t }
[@@deriving sexp, compare, equal]

let rec find_cheat_ends ~cache ~field pos power =
  if Hashtbl.mem cache (pos, power) then Hashtbl.find_exn cache (pos, power)
  else
    let res =
      if power = 0 then
        (* No more cheat power. Stopping at safe place *)
        if Myfield.eq_at ~field ~c:'#' pos then [] else [ (pos, 0) ]
      else
        (* Find all possible ends for the neighbours *)
        let neighbors_ends =
          Myfield.Point.neighbors pos
          |> List.filter ~f:(Myfield.contains field)
          |> List.concat_map ~f:(fun n ->
                 find_cheat_ends ~cache ~field n (power - 1))
        in
        (* As we do not try be optimal here -- for each end find a minimal distance to it *)
        let best_ends =
          neighbors_ends
          |> List.fold
               ~init:(Hashtbl.create (module Myfield.Point))
               ~f:(fun acc (point, dist) ->
                 Hashtbl.update acc point ~f:(function
                   | None -> dist
                   | Some existing_dist -> Int.min existing_dist dist);
                 acc)
          |> Hashtbl.to_alist
          |> List.map ~f:(fun (p, d) -> (p, d + 1))
        in
        (* Also we don't need to exhhaust the cheat power, so if we can stop here -- stop *)
        if Myfield.eq_at ~field ~c:'#' pos then best_ends
        else (pos, 0) :: best_ends
    in
    Hashtbl.set cache ~key:(pos, power) ~data:res;
    res

let find_ways field ~distances ~from ~fair_best ~cheat_power =
  let res = Hashtbl.Poly.create () in
  let cache = Hashtbl.Poly.create () in
  let rec dfs state =
    (* We too deep -- cut *)
    if state.steps >= fair_best then ()
    else
      let neighbors =
        Myfield.Point.neighbors state.pos
        |> List.filter ~f:(Myfield.contains field)
        |> List.filter ~f:(fun pos -> not (Set.mem state.visited pos))
      in

      (* Go fair *)
      neighbors
      |> List.filter ~f:(fun pos -> not (Myfield.eq_at ~field ~c:'#' pos))
      |> List.iter ~f:(fun n ->
             dfs
               {
                 pos = n;
                 steps = state.steps + 1;
                 visited = Set.add state.visited n;
               });

      (* Go cheat *)
      let cheat_ends = find_cheat_ends ~cache ~field state.pos cheat_power in
      List.iter cheat_ends ~f:(fun (pos, dist) ->
          let finish_distance =
            Hashtbl.find_exn distances pos + state.steps + dist
          in
          if finish_distance < fair_best then
            Hashtbl.add_exn res
              ~key:{ start = state.pos; stop = pos }
              ~data:finish_distance)
  in
  dfs { steps = 0; pos = from; visited = Myfield.PointSet.of_list [ from ] };
  res

let solve ~cheat_power fname =
  let field = Myfield.read fname in
  let start = Myfield.find field ~f:(Char.equal 'S') |> List.hd_exn in
  let finish = Myfield.find field ~f:(Char.equal 'E') |> List.hd_exn in
  let distances = Myfield.calc_distances field ~start:finish in
  let fair_best = Hashtbl.find_exn distances start in

  find_ways field ~distances ~from:start ~fair_best ~cheat_power
  |> Hashtbl.Poly.to_alist
  |> List.map ~f:(fun (_cheat, dist) -> dist)
  |> List.count ~f:(fun d -> fair_best - d >= 100)

let () = assert (solve ~cheat_power:2 "data/day20.txt" = 1454)
let () = assert (Util.time (solve ~cheat_power:20) "data/day20.txt" = 997879)
