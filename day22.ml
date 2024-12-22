open Core

let prune n = n land ((1 lsl 24) - 1)
let mix a b = a lxor b

let next n =
  let n = mix n (n lsl 6) |> prune in
  let n = mix n (n lsr 5) |> prune in
  let n = mix n (n lsl 11) |> prune in
  n

let generate ~n ~init =
  let seq = Sequence.unfold ~init ~f:(fun n -> Some (n, next n)) in
  Sequence.take seq (n + 1)

let () =
  assert (
    List.equal Int.equal
      [
        123;
        15887950;
        16495136;
        527345;
        704524;
        1553684;
        12683156;
        11100544;
        12249484;
        7753432;
        5908254;
      ]
      (generate ~init:123 ~n:10 |> Sequence.to_list))

let nth ~n x = Sequence.nth_exn (generate ~init:x ~n) n
let () = assert (nth ~n:2000 1 = 8685429)

let solve1 fname =
  let fname = fname in
  In_channel.read_lines fname
  |> List.map ~f:int_of_string
  |> List.map ~f:(nth ~n:2000)
  |> List.reduce_exn ~f:( + )

let () =
  assert (solve1 "test/day22.txt" = 37327623);
  assert (solve1 "data/day22.txt" = 14726157693)

let price_changes lst =
  let init = List.hd_exn lst mod 10 in
  List.drop lst 1
  |> List.fold_map ~init ~f:(fun prev curr ->
         let digit = curr mod 10 in
         (digit, digit - prev))
  |> snd

let () =
  assert (
    List.equal Int.equal
      [ -3; 6; -1; -1; 0; 2; -2; 0; -2 ]
      (generate ~init:123 ~n:9 |> Sequence.to_list |> price_changes))

let sliding_windows ~n seq =
  let first_window = Sequence.take seq n |> Sequence.to_list in
  if List.length first_window < n then Sequence.empty
  else
    let tail = Sequence.drop seq n in
    let tail_with_fake_last = Sequence.append tail (Sequence.singleton 0) in
    Sequence.unfold ~init:(first_window, tail_with_fake_last)
      ~f:(fun (window, rest) ->
        match Sequence.next rest with
        | None -> None
        | Some (x, next_rest) ->
            let next_window = List.tl_exn window @ [ x ] in
            Some (window, (next_window, next_rest)))

let () =
  assert (
    Sequence.equal (List.equal Int.equal)
      (Sequence.of_list [ [ 1; 2; 3 ]; [ 2; 3; 4 ]; [ 3; 4; 5 ] ])
      (Sequence.of_list [ 1; 2; 3; 4; 5 ] |> sliding_windows ~n:3))

(* Each diff can be [-9; +9] so we add 10 to make them positive and then calculate base20 number *)
let key_of_diffs =
  List.fold_left ~init:0 ~f:(fun acc x -> (acc * 20) + (x + 10))

let solve2 fname =
  let prefix_values = Hashtbl.create (module Int) in

  let solve seed =
    let seen = Hashtbl.create (module Int) in

    generate ~init:seed ~n:2000
    |> sliding_windows ~n:(1 + 4)
    (* 1 from previous window + 4 in window to calc 4-diffs sequence *)
    |> Sequence.iter ~f:(fun window ->
           let key = window |> price_changes |> key_of_diffs in
           if not (Hashtbl.mem seen key) then (
             Hashtbl.set seen ~key ~data:();
             let value = List.last_exn window mod 10 in
             Hashtbl.update prefix_values key ~f:(function
               | None -> value
               | Some old -> old + value)))
  in

  In_channel.read_lines fname |> List.map ~f:int_of_string |> List.iter ~f:solve;

  Hashtbl.to_alist prefix_values
  |> List.max_elt ~compare:(fun (_, x) (_, y) -> Int.compare x y)
  |> Option.value_exn |> snd

let () =
  assert (solve2 "test/day22-2.txt" = 23);
  assert (solve2 "data/day22.txt" = 1614)
