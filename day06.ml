open Core

module Point = struct
  type t = int * int [@@deriving compare, sexp]

  let to_string p = p |> sexp_of_t |> Sexp.to_string_hum
end

let read_field file =
  In_channel.read_lines file |> List.map ~f:String.to_array |> List.to_array

let get field (i, j) = try Some field.(i).(j) with Invalid_argument _ -> None

let find_start field =
  Array.find_mapi field ~f:(fun i row ->
      Array.find_mapi row ~f:(fun j elem ->
          if Char.equal elem '^' then Some (i, j) else None))
  |> Option.value_exn

let () =
  assert (Point.compare (find_start (read_field "test/day06.txt")) (6, 4) = 0)

let turn d =
  match d with
  | '^' -> '>'
  | '>' -> 'v'
  | 'v' -> '<'
  | '<' -> '^'
  | _ -> raise (Failure "unreachable")

let step d (x, y) =
  match d with
  | '^' -> (x - 1, y)
  | '>' -> (x, y + 1)
  | 'v' -> (x + 1, y)
  | '<' -> (x, y - 1)
  | _ -> raise (Failure "unreachable")

let traverse field ~from:(x, y) =
  field.(x).(y) <- '.';
  let rec loop visited dir pos =
    let next_pos = step dir pos in
    match get field next_pos with
    | None -> pos :: visited
    | Some '#' -> loop visited (turn dir) pos
    | Some '.' -> pos :: loop visited dir next_pos
    | _ ->
        raise
          (Failure (Format.sprintf "Strange position %s" (Point.to_string pos)))
  in
  loop [] '^' (x, y)

module PointsSet = Set.Make (Point)

let solve1 fname =
  let field = read_field fname in
  let start = find_start field in
  traverse field ~from:start |> PointsSet.of_list |> Set.length

let () =
  assert (solve1 "test/day06.txt" = 41);
  assert (solve1 "data/day06.txt" = 4982)

module PointWithDir = struct
  type t = char * int * int [@@deriving compare, sexp]

  let to_string p = p |> sexp_of_t |> Sexp.to_string_hum
end

module VisitedSet = Set.Make (PointWithDir)

let has_loop field ~from:(x, y) =
  field.(x).(y) <- '.';
  let visited = ref VisitedSet.empty in
  let rec loop dir pos =
    if Set.mem !visited (dir, fst pos, snd pos) then true
    else
      let next_pos = step dir pos in
      match get field next_pos with
      | None -> false
      | Some '#' -> loop (turn dir) pos
      | Some '.' ->
          visited := Set.add !visited (dir, fst pos, snd pos);
          loop dir next_pos
      | _ ->
          raise
            (Failure
               (Format.sprintf "Strange position %s" (Point.to_string pos)))
  in
  loop '^' (x, y)

let _ =
  let field = read_field "test/day06.txt" in
  let start = find_start field in
  assert (not (has_loop field ~from:start))

let solve2 fname =
  let field = read_field fname in
  let start = find_start field in
  let default_path = traverse field ~from:start in
  default_path |> List.tl_exn
  |> List.filter ~f:(fun (x, y) ->
         (* let new_field = Array.copy_matrix field in *)
         field.(x).(y) <- '#';
         let res = has_loop field ~from:start in
         field.(x).(y) <- '.';
         res
         )
  |> PointsSet.of_list |> Set.length

let time f x =
  let start = Time_float.now() in
  let fx = f x in
  let elapsed = Time_float.diff (Time_float.now()) start in
  Printf.printf "Execution time: %s\n" (Time_float.Span.to_string_hum elapsed);
  fx

let () =
  assert (time solve2 "test/day06.txt" = 6);
  assert (time solve2 "data/day06.txt" = 1663)
