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
  | _ -> failwith "unreachable"

let step d (x, y) =
  match d with
  | '^' -> (x - 1, y)
  | '>' -> (x, y + 1)
  | 'v' -> (x + 1, y)
  | '<' -> (x, y - 1)
  | _ -> failwith "unreachable"

let traverse field ~from:(x, y) =
  field.(x).(y) <- '.';
  let rec loop visited dir pos =
    let next_pos = step dir pos in
    match get field next_pos with
    | None -> pos :: visited
    | Some '#' -> loop visited (turn dir) pos
    | Some '.' -> pos :: loop visited dir next_pos
    | _ -> failwithf "Strange position %s" (Point.to_string pos) ()
  in
  loop [] '^' (x, y)

let solve1 fname =
  let field = read_field fname in
  let start = find_start field in
  traverse field ~from:start
  |> List.dedup_and_sort ~compare:Point.compare
  |> List.length

let () =
  assert (solve1 "test/day06.txt" = 41);
  assert (solve1 "data/day06.txt" = 4982)

module PointWithDir = struct
  type t = (int * int) * char [@@deriving compare, sexp]

  let to_string p = p |> sexp_of_t |> Sexp.to_string_hum
end

module VisitedSet = Set.Make (PointWithDir)

let traverse_seq field ~from =
  Sequence.unfold ~init:(from, '^') ~f:(fun (pos, dir) ->
      let next_pos = step dir pos in
      match get field next_pos with
      | None -> None
      | Some '#' -> Some ((pos, dir), (pos, turn dir))
      | Some '.' | Some '^' -> Some ((pos, dir), (next_pos, dir))
      | _ -> failwithf "Strange position %s" (Point.to_string pos) ())

let has_loop field ~from =
  traverse_seq field ~from
  |> Sequence.fold_result ~init:VisitedSet.empty ~f:(fun visited pos ->
         if Set.mem visited pos then Error "break" else Ok (Set.add visited pos))
  |> Result.is_error

let () =
  let field = read_field "test/day06.txt" in
  let start = find_start field in
  assert (not (has_loop field ~from:start))

let solve2 fname =
  let field = read_field fname in
  let start = find_start field in
  let default_path = traverse field ~from:start in
  List.drop default_path 1 (* Skip start *)
  |> List.dedup_and_sort ~compare:Point.compare
  |> List.count ~f:(fun (x, y) ->
         field.(x).(y) <- '#';
         let res = has_loop field ~from:start in
         field.(x).(y) <- '.';
         res)

let () = assert (solve2 "test/day06.txt" = 6)

(* Slow and executes on every utop open *)
(*
  let () = assert (Util.time solve2 "data/day06.txt" = 1663) 
*)
