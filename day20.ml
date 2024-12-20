open Core

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

type cheat = { start : Myfield.Point.t; stop : Myfield.Point.t }
[@@deriving sexp, compare, equal]

let find_ways field ~distances ~from ~fair_best ~cheat_power =
  let res = Hashtbl.Poly.create () in
  let cache = Hashtbl.Poly.create () in
  let rec dfs pos steps visited =
    (* We too deep -- cut that end *)
    if steps >= fair_best then ()
    else
      let neighbors =
        Myfield.Point.neighbors pos
        |> List.filter ~f:(Myfield.contains field)
        |> List.filter ~f:(fun pos -> not (Set.mem visited pos))
      in

      (* Go fair: just DFS avoiding walls *)
      neighbors
      |> List.filter ~f:(fun pos -> not (Myfield.eq_at ~field ~c:'#' pos))
      |> List.iter ~f:(fun n -> dfs n (steps + 1) (Set.add visited n));

      (* Go cheat: recursive call with less cheat power and memoization*)
      let cheat_ends = find_cheat_ends ~cache ~field pos cheat_power in
      List.iter cheat_ends ~f:(fun (end_pos, spent_in_cheat) ->
          let from_end_pos_to_finish = Hashtbl.find_exn distances end_pos in
          let steps_to_finish =
            steps + spent_in_cheat + from_end_pos_to_finish
          in
          if steps_to_finish < fair_best then
            Hashtbl.add_exn res
              ~key:{ start = pos; stop = end_pos }
              ~data:steps_to_finish)
  in
  dfs from 0 (Myfield.PointSet.of_list [ from ]);
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
(* let () = assert (Util.time (solve ~cheat_power:20) "data/day20.txt" = 997879) *)
