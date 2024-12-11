open Core

let num_digits n = int_of_float (Float.log10 (float_of_int n)) + 1

let split_int_by_halves n =
  let half_digits = Int.pow 10 (num_digits n / 2) in
  (n / half_digits, n mod half_digits)

let rec count_stone =
  let cache = Hashtbl.Poly.create () in
  let rec count n stone =
    let res =
      let cached = Hashtbl.Poly.find cache (stone, n) in
      match (cached, stone, n) with
      | Some cached, _, _ -> cached
      | _, _, 0 -> 1
      | _, 0, n -> count (n - 1) 1
      | _, stone, n when num_digits stone mod 2 = 0 ->
          let left, right = split_int_by_halves stone in
          count (n - 1) left + count (n - 1) right
      | _, stone, n -> count (n - 1) (stone * 2024)
    in
    Hashtbl.Poly.set cache ~key:(stone, n) ~data:res;
    res
  in
  count

let solve filename n =
  In_channel.read_all filename
  |> Re.split (Re.compile Re.blank)
  |> List.map ~f:int_of_string
  |> List.map ~f:(count_stone n)
  |> List.reduce_exn ~f:( + )

let () =
  assert (solve "test/day11.txt" 25 = 55312);
  assert (solve "data/day11.txt" 25 = 203457);
  assert (solve "data/day11.txt" 75 = 241394363462435)
