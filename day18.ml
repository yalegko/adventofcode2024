open Core

let read_packets fname =
  In_channel.read_lines fname
  |> List.map ~f:(fun line ->
         String.split line ~on:',' |> List.map ~f:int_of_string)
  |> List.map ~f:(fun cords ->
         match cords with
         | [ y; x ] -> (x, y)
         | _ -> failwith "unexpected input")

let build_map (n, m) cords =
  let field = Array.make_matrix ~dimx:(n + 1) ~dimy:(m + 1) '.' in
  List.iter cords ~f:(fun (x, y) -> field.(x).(y) <- '#');
  field

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
          |> List.filter ~f:(fun n -> Option.exists ~f:(Char.equal '.') (Myfield.get field n))
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

let solve1 fname size npackets =
  let packets = read_packets fname in
  let field = build_map size (List.take packets npackets) in
  let distances = calc_distances field ~start:(0,0) in
  Hashtbl.find distances size
  |> Option.value_exn 
  

let () =
  assert (solve1 "test/day18.txt" (6,6) 12 = 22);
  assert (solve1 "data/day18.txt" (70,70) 1024 = 264)


let solve2 fname size =
  let packets = read_packets fname in
  
  let (x,y) =
  Sequence.range (List.length packets) 1 ~stride:(-1)
  |> Sequence.find_exn ~f:(fun npackets -> 
    let field = build_map size (List.take packets npackets) in
    let distances = calc_distances field ~start:(0,0) in
    Hashtbl.find distances size |> Option.is_some)
  |> List.nth_exn packets in

  sprintf "%d,%d" y x

let () =
  assert (String.equal (solve2 "test/day18.txt" (6,6))  "6,1");
  assert (String.equal (solve2 "data/day18.txt" (70,70)) "41,26")