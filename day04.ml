open Core

let read_field file =
  In_channel.read_lines file |> List.map ~f:String.to_array |> List.to_array

let get field (i, j) = try Some field.(i).(j) with Invalid_argument _ -> None

let opt_list_eq opt_list val_list =
  let opt_list2 = val_list |> List.map ~f:Option.some in
  List.equal (Option.equal Char.equal) opt_list opt_list2

let is_xmas opt_list = opt_list_eq opt_list [ 'X'; 'M'; 'A'; 'S' ]

let () =
  assert (is_xmas [ Some 'X'; Some 'M'; Some 'A'; Some 'S' ]);
  assert (not (is_xmas [ None; Some 'M'; Some 'A'; Some 'S' ]))

let can_read_xmas field (i, j) =
  let same v _ = v in
  let direction_score (dx, dy) =
    [ (i, j); (dx i 1, dy j 1); (dx i 2, dy j 2); (dx i 3, dy j 3) ]
    |> List.map ~f:(get field)
    |> is_xmas |> Bool.to_int
  in
  [
    (* Vertical *)
    (( + ), same);
    (( - ), same);
    (* Horizontal *)
    (same, ( + ));
    (same, ( - ));
    (* Diagonal *)
    (( + ), ( + ));
    (( + ), ( - ));
    (( - ), ( + ));
    (( - ), ( - ));
  ]
  |> List.map ~f:direction_score
  |> List.reduce_exn ~f:( + )

let traverse cb field =
  let n = Array.length field and m = Array.length field.(0) in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      cb field (i, j)
    done
  done

let solve1 file =
  let cnt = ref 0 in
  let count_xmas field (i, j) =
    if Char.equal field.(i).(j) 'X' then cnt := !cnt + can_read_xmas field (i, j)
  in
  read_field file |> traverse count_xmas;
  !cnt

let () =
  assert (solve1 "test/day04.txt" = 18);
  assert (solve1 "data/day04.txt" = 2447)

let is_x_form field (i, j) =
  let get_chars list = List.map list ~f:(get field) in
  let elements_match l1 l2 =
    opt_list_eq l1 l2 || opt_list_eq l1 (List.rev l2)
  in
  let d1 = get_chars [ (i - 1, j - 1); (i + 1, j + 1) ]
  and d2 = get_chars [ (i + 1, j - 1); (i - 1, j + 1) ] in
  elements_match d1 [ 'M'; 'S' ] && elements_match d2 [ 'M'; 'S' ]

let solve2 file =
  let cnt = ref 0 in
  let count_xmas field (i, j) =
    if Char.equal field.(i).(j) 'A' && is_x_form field (i, j) then
      cnt := !cnt + 1
  in
  read_field file |> traverse count_xmas;
  !cnt

let () =
  assert (solve2 "test/day04.txt" = 9);
  assert (solve2 "data/day04.txt" = 1868)
