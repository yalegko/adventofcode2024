open Core

type block = File of int * int | Space of int
[@@deriving compare, equal, sexp]

let is_empty_space = function Space 0 -> true | _ -> false

let read_disk fname =
  In_channel.read_all fname |> String.to_list
  |> List.folding_map ~init:(true, 0) ~f:(fun (is_file, id) ch ->
         let amount = int_of_char ch - int_of_char '0' in
         let block = if is_file then File (id, amount) else Space amount in
         let next_id = if is_file then id + 1 else id in
         ((not is_file, next_id), block))
  |> List.filter ~f:(Fun.negate is_empty_space)

let defragment_one blocks =
  let empty_size, tail =
    match blocks with
    | [] -> (-1, [])
    | Space s :: tail -> (s, tail)
    | _ -> failwith "unexpected first block"
  in
  let last, but_last =
    match List.rev tail with
    | [] -> (None, [])
    | x :: rest -> (Some x, List.rev rest)
  in
  match last with
  | None -> []
  | Some (Space _) -> Space empty_size :: but_last
  | Some (File (id, fsize)) when empty_size = fsize ->
      [ File (id, fsize) ] @ but_last
  | Some (File (id, fsize)) when empty_size > fsize ->
      [ File (id, fsize); Space (empty_size - fsize) ] @ but_last
  | Some (File (id, fsize)) when empty_size < fsize ->
      [ File (id, empty_size) ] @ but_last @ [ File (id, fsize - empty_size) ]
  | _ -> failwith "unreachable"

let () =
  assert (
    List.equal equal_block
      (defragment_one [ Space 1; File (1337, 7); File (1339, 1) ])
      [ File (1339, 1); File (1337, 7) ]);
  assert (
    List.equal equal_block
      (defragment_one [ Space 5; File (1337, 7); File (1339, 1) ])
      [ File (1339, 1); Space 4; File (1337, 7) ]);
  assert (
    List.equal equal_block
      (defragment_one [ Space 1; File (1337, 7); File (1339, 4) ])
      [ File (1339, 1); File (1337, 7); File (1339, 3) ])

let defragment blocks =
  let rec loop res rest =
    match rest with
    | [] -> List.rev res
    | (File _ as f) :: tail -> loop (f :: res) tail
    | Space _ :: _tail -> (
        match defragment_one rest with
        | [] -> List.rev res
        | (File _ as f) :: tail -> loop (f :: res) tail
        (* if we have empty block at the end of the tail -- iterate once more w/o it *)
        | (Space _ as s) :: tail -> loop res (s :: tail))
  in
  loop [] blocks

let hash blocks =
  let rec sum id i n = if n = 0 then 0 else (id * i) + sum id (i + 1) (n - 1) in
  List.fold blocks ~init:(0, 0) ~f:(fun (res, i) block ->
      let new_idx, s =
        match block with
        | Space size -> (i + size, 0)
        | File (id, size) -> (i + size, sum id i size)
      in
      (res + s, new_idx))
  |> fst

let solve1 fname = read_disk fname |> defragment |> hash
let () = assert (solve1 "test/day09.txt" = 1928)
(* Slowish *)
(* let () = assert (Util.time solve1 "data/day09.txt" = 6435922584968) *)

let insert_block blocks f =
  let id, fsize =
    match f with File (i, s) -> (i, s) | _ -> failwith "can't insert Space"
  in
  let insert_to =
    List.findi blocks ~f:(fun _i b ->
        match b with
        | Space empty_size when empty_size >= fsize -> true
        | _ -> false)
  in
  match insert_to with
  | None -> None
  | Some (i, Space empty_size) ->
      Some
        (List.concat
           [
             List.take blocks i;
             [ File (id, fsize); Space (empty_size - fsize) ];
             List.drop blocks (i + 1);
             [ Space fsize ] (* empty space from the file *);
           ])
  | _ -> failwith "unreachable"

let defragment2 blocks =
  let rec loop ~blocks n =
    let idx = List.length blocks + n in
    match List.nth blocks idx with
    | None -> blocks
    | Some (Space _) -> loop ~blocks (n - 1)
    | Some (File (_, _) as f) -> (
        match insert_block (List.take blocks idx) f with
        | None -> loop ~blocks (n - 1)
        | Some head ->
            let tail = List.drop blocks (idx + 1) in
            loop ~blocks:(List.append head tail) (n - 1))
  in
  loop ~blocks (-1)

let solve2 fname = read_disk fname |> defragment2 |> hash
let () = assert (solve2 "test/day09.txt" = 2858)
(* Slowish *)
(* let () = assert (Util.time solve2 "data/day09.txt" = 6469636832766) *)
