open Core

let parse line =
  match String.split line ~on:':' with
  | [ res; terms ] ->
      ( int_of_string res,
        terms |> Re.split (Re.compile Re.blank) |> List.map ~f:int_of_string )
  | _ -> failwith "Unexpected line format"

let can_be_solved (res, terms) =
  let rec loop acc rest_terms =
    if acc > res then false
    else
      match rest_terms with
      | [] -> acc = res
      | x :: tail -> loop (acc + x) tail || loop (acc * x) tail
  in
  loop (List.hd_exn terms) (List.tl_exn terms)

let () =
  assert ("190: 10 19" |> parse |> can_be_solved);
  assert ("3267: 81 40 27" |> parse |> can_be_solved);
  assert ("292: 11 6 16 20" |> parse |> can_be_solved);
  assert (not ("83: 17 5" |> parse |> can_be_solved))

let solve1 fname =
  In_channel.read_lines fname
  |> List.map ~f:parse
  |> List.filter ~f:can_be_solved
  |> List.map ~f:fst |> List.reduce_exn ~f:( + )

let () =
  assert (solve1 "test/day07.txt" = 3749);
  assert (solve1 "data/day07.txt" = 1620690235709)

let concat_numbers x y = string_of_int x ^ string_of_int y |> int_of_string

let can_be_solved2 (res, terms) =
  let rec loop acc rest_terms =
    if acc > res then false
    else
      match rest_terms with
      | [] -> acc = res
      | x :: tail ->
          loop (acc + x) tail
          || loop (acc * x) tail
          || loop (concat_numbers acc x) tail
  in
  loop (List.hd_exn terms) (List.tl_exn terms)

let () =
  assert ("190: 10 19" |> parse |> can_be_solved2);
  assert ("3267: 81 40 27" |> parse |> can_be_solved2);
  assert ("292: 11 6 16 20" |> parse |> can_be_solved2);

  assert ("156: 15 6" |> parse |> can_be_solved2);
  assert ("7290: 6 8 6 15" |> parse |> can_be_solved2);
  assert ("292: 11 6 16 20" |> parse |> can_be_solved2);

  assert (not ("83: 17 5" |> parse |> can_be_solved))

let solve2 fname =
  In_channel.read_lines fname
  |> List.map ~f:parse
  |> List.filter ~f:can_be_solved2
  |> List.map ~f:fst |> List.reduce_exn ~f:( + )

let () =
  assert (solve2 "test/day07.txt" = 11387);
  assert (solve2 "data/day07.txt" = 145397611075341)
