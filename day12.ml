open Core

let union_find_regions field =
  let n = Array.length field and m = Array.length field.(0) in

  (* Create a uninon-find thingy *)
  let uf =
    Array.init n ~f:(fun i ->
        Array.init m ~f:(fun j -> Union_find.create (i, j)))
  in
  let is_same_plant p1 p2 =
    Option.equal Char.equal (Myfield.get field p1) (Myfield.get field p2)
  in

  (* Perform unions *)
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      if is_same_plant (i, j) (i + 1, j) then
        Union_find.union uf.(i).(j) uf.(i + 1).(j);
      if is_same_plant (i, j) (i, j + 1) then
        Union_find.union uf.(i).(j) uf.(i).(j + 1)
    done
  done;

  (* Join union by their eq-classes *)
  let regions = Hashtbl.create (module Myfield.Point) in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      let plant_type = Union_find.get uf.(i).(j) in
      Hashtbl.add_multi regions ~key:plant_type ~data:(i, j)
    done
  done;

  Hashtbl.data regions

let region_perimeter field region =
  let plant_type = Myfield.get field (List.hd_exn region) in
  let num_fences p =
    Myfield.Point.neighbors p
    |> List.map ~f:(Myfield.get field)
    |> List.count ~f:(Fun.negate (Option.equal Char.equal plant_type))
  in
  List.map region ~f:num_fences |> List.reduce_exn ~f:( + )

let solve1 fname =
  let field = Myfield.read fname in
  union_find_regions field
  |> List.map ~f:(fun r -> List.length r * region_perimeter field r)
  |> List.reduce_exn ~f:( + )

let () =
  assert (solve1 "test/day12.txt" = 1930);
  assert (solve1 "data/day12.txt" = 1361494)

let num_corners region =
  let region_points = Myfield.PointSet.of_list region in
  let num_region_intersections (x, y) direction =
    List.map direction ~f:(fun (dx, dy) -> (x + dx, y + dy))
    |> List.count ~f:(Set.mem region_points)
  in

  let c1 = [ (0, -1); (-1, -1); (-1, 0) ] (* top left corner *)
  and c2 = [ (-1, 0); (-1, 1); (0, 1) ] (* top right corner *)
  and c3 = [ (0, 1); (1, 1); (1, 0) ] (* down right corner *)
  and c4 = [ (1, 0); (1, -1); (0, -1) ] (* down left corner *) in
  let num_point_corners p =
    [ c1; c2; c3; c4 ]
    |> List.count ~f:(fun direction ->
           let diagonal = List.nth_exn direction 1 in
           let intersection = num_region_intersections p direction in
           let is_outer_corner = intersection = 0
           and is_inner_corner =
             intersection = 2 && num_region_intersections p [ diagonal ] = 0
           and touching_corner =
             intersection = 1 && num_region_intersections p [ diagonal ] = 1
           in
           is_inner_corner || is_outer_corner || touching_corner)
  in
  List.map region ~f:num_point_corners |> List.reduce_exn ~f:( + )

let () =
  assert (num_corners [ (0, 0); (1, 0); (2, 0) ] = 4);
  assert (num_corners [ (0, 0); (1, 0); (0, 1); (1, 1) ] = 4);
  assert (num_corners [ (0, 0); (1, 0); (2, 0); (2, 1) ] = 6);
  assert (num_corners [ (0, 0); (1, 0); (2, 0); (2, 1); (2, 2); (1, 2) ] = 8)

let field_price field =
  union_find_regions field
  |> List.map ~f:(fun r -> List.length r * num_corners r)
  |> List.reduce_exn ~f:( + )

let () =
  let field = Myfield.of_string "AAAA\nBBCD\nBBCC\nEEEC" in
  assert (field_price field = 80);
  let field = Myfield.of_string "EEEEE\nEXXXX\nEEEEE\nEXXXX\nEEEEE" in
  assert (field_price field = 236);
  let field =
    Myfield.of_string "AAAAAA\nAAABBA\nAAABBA\nABBAAA\nABBAAA\nAAAAAA"
  in
  assert (field_price field = 368)

let solve2 fname = Myfield.read fname |> field_price

let () =
  assert (solve2 "test/day12.txt" = 1206);
  assert (solve2 "data/day12.txt" = 830516)
