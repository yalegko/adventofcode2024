open Core

let re_findall re str =
  let re = Re.compile re in
  let matches =
    Re.all re str |> List.map ~f:Re.Group.all |> List.map ~f:Array.to_list
  in
  match Re.group_count re with
  | 1 -> matches
  | _ -> List.map ~f:(fun groups -> List.drop groups 1) matches

let re_findall_s re str = re_findall (Re.Pcre.re re) str

let () =
  assert (
    List.equal (List.equal String.equal)
      (re_findall_s "[0-9]+" "123|345")
      [ [ "123" ]; [ "345" ] ]);
  assert (
    List.equal (List.equal String.equal)
      (re_findall_s "[0-9]+" "123345")
      [ [ "123345" ] ]);
  assert (
    List.equal (List.equal String.equal)
      (re_findall_s {|(\w+)\((\d+),(\d+)\)|} "foo(1,2) bar(1337,4)")
      [ [ "foo"; "1"; "2" ]; [ "bar"; "1337"; "4" ] ])

let mul_re = {|(mul)\((\d{1,3}),(\d{1,3})\)|}

let solve1 file =
  In_channel.read_all file |> re_findall_s mul_re |> List.map ~f:List.tl_exn
  |> List.map ~f:(List.map ~f:int_of_string)
  |> List.map ~f:(List.fold_left ~f:( * ) ~init:1)
  |> List.fold_left ~f:( + ) ~init:0

let () =
  assert (solve1 "test/day03.txt" = 161);
  assert (solve1 "data/day03.txt" = 167090022)

let calculate program =
  let res = ref 0 and doing = ref true in
  List.iter program ~f:(fun instr ->
      match instr with
      | [ "do" ] -> doing := true
      | [ "don't" ] -> doing := false
      | [ "mul"; a; b ] ->
          if !doing then res := !res + (int_of_string a * int_of_string b)
      | _ -> raise (Failure "unexpected instruction"));
  !res

let solve2 file =
  let re =
    Re.alt
      [ Re.Pcre.re mul_re; Re.Pcre.re {|(do)\(\)|}; Re.Pcre.re {|(don't)\(\)|} ]
  in
  In_channel.read_all file |> re_findall re
  |> List.map ~f:(List.filter ~f:(fun s -> not (String.is_empty s)))
  |> calculate

let () =
  assert (solve2 "test/day03-2.txt" = 48);
  assert (solve2 "data/day03.txt" = 89823704)
