open Core

let parse file =
  In_channel.read_lines file
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(List.map ~f:int_of_string)

let rec check_ordered cmp = function
  | [] -> true
  | [ _ ] -> true
  | a :: b :: tail -> cmp a b && check_ordered cmp (b :: tail)

let asc x y =
  let diff = y - x in
  diff >= 1 && diff <= 3

let desc x y = asc y x

let () =
  assert (check_ordered asc [ 1; 2; 3 ]);
  assert (check_ordered asc [ 1; 4; 7 ]);
  assert (not (check_ordered asc [ 4; 2; 3 ]));
  assert (not (check_ordered asc [ 1; 5; 6 ]));

  assert (check_ordered desc [ 3; 2; 1 ]);
  assert (check_ordered desc [ 7; 4; 1 ]);
  assert (not (check_ordered desc [ 1; 2; 3 ]));
  assert (not (check_ordered desc [ 4; 2; 3 ]));
  assert (not (check_ordered desc [ 6; 5; 1 ]))

let is_good_seq seq =
  match seq with
  | a :: b :: _ when a < b -> check_ordered asc seq
  | a :: b :: _ when a > b -> check_ordered desc seq
  | _ -> false

let () =
  assert (is_good_seq [ 1; 2; 3 ]);
  assert (is_good_seq [ 3; 2; 1 ]);
  assert (not (is_good_seq [ 1; 5; 6 ]))

let solve1 fname = parse fname |> List.count ~f:is_good_seq

let () =
  assert (solve1 "test/day02.txt" = 2);
  assert (solve1 "data/day02.txt" = 502)

(* part2 *)

let is_good_seq2 seq =
  let is_good_without idx =
    seq |> List.filteri ~f:(fun i _ -> i <> idx) |> is_good_seq
  in
  seq |> List.findi ~f:(fun i _ -> is_good_without i) |> is_some

let () =
  assert (is_good_seq2 [ 7; 6; 4; 2; 1 ]);
  assert (is_good_seq2 [ 1; 3; 6; 7; 9 ]);

  assert (is_good_seq2 [ 1; 3; 2; 4; 5 ]);
  assert (is_good_seq2 [ 8; 6; 4; 4; 1 ]);

  assert (not (is_good_seq2 [ 1; 2; 7; 8; 9 ]));
  assert (not (is_good_seq2 [ 9; 7; 6; 2; 1 ]))

let solve2 fname = parse fname |> List.count ~f:is_good_seq2

let () =
  assert (solve2 "test/day02.txt" = 4);
  assert (solve2 "data/day02.txt" = 544)
