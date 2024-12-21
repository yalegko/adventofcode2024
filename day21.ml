open Core

let numpad =
  [|
    [| '7'; '8'; '9' |];
    [| '4'; '5'; '6' |];
    [| '1'; '2'; '3' |];
    [| ' '; '0'; 'A' |];
  |]

let keypad = [| [| ' '; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]

let pad_neighbours ~pad pos =
  Myfield.Point.neighbors pos
  |> List.filter ~f:(Myfield.contains pad)
  |> List.filter ~f:(fun n -> not (Myfield.eq_at ~field:pad ~c:' ' n))

let find_all_shortest_paths ~neighbors ~start ~finish =
  let paths = ref [] in
  let shortest = ref 100500 in

  let visited = Hashtbl.create (module Myfield.Point) in
  let rec dfs path =
    match List.hd_exn path with
    | pos when Hashtbl.mem visited pos -> ()
    | pos when Myfield.Point.equal pos finish -> (
        match List.length path with
        | l when l < !shortest ->
            shortest := List.length path;
            paths := [ path ]
        | l when l = !shortest -> paths := path :: !paths
        | _ -> ())
    | pos ->
        Hashtbl.set visited ~key:pos ~data:();
        List.iter (neighbors pos) ~f:(fun n -> dfs (n :: path));
        Hashtbl.remove visited pos
  in

  dfs [ start ];
  List.map !paths ~f:List.rev

let path_to_segments path =
  (* [1; 2; 3] -> [[1; 2]; [2; 3]] *)
  List.drop path 1
  |> List.fold_left
       ~init:([], List.hd_exn path)
       ~f:(fun (res, prev) v -> ((prev, v) :: res, v))
  |> fst |> List.rev

let path_to_keys path =
  (* Calculates keystrokes from a list of points in path *)
  path_to_segments path
  |> List.fold_left ~init:[] ~f:(fun keys ((x, y), (x', y')) ->
         match (x' - x, y' - y) with
         | 1, 0 -> 'v' :: keys
         | -1, 0 -> '^' :: keys
         | 0, 1 -> '>' :: keys
         | 0, -1 -> '<' :: keys
         | dx, dy -> failwithf "Invalid path %d %d" dx dy ())
  |> List.cons 'A' |> List.rev

let pad_coord ~pad ch =
  Myfield.find pad ~f:(fun c -> Char.equal c ch) |> List.hd_exn

let keypad_paths ~pad sequence =
  (* Takes an input pad and produces possible ways to press arrows to enter @sequence *)
  let neighbors = pad_neighbours ~pad in
  path_to_segments ('A' :: sequence)
  |> List.map ~f:(fun (from', to') ->
         let start = pad_coord ~pad from' in
         let finish = pad_coord ~pad to' in
         find_all_shortest_paths ~neighbors ~start ~finish)
  |> List.map ~f:(fun lst -> List.map lst ~f:path_to_keys)

let min_key_presses =
  (*
     Calculates a minimum number of keys to press to enter an sequence of keypad keys
     using @depth levels of connected keypads.
  *)
  let cache = Hashtbl.Poly.create () in
  let rec loop ~depth arrows =
    match Hashtbl.Poly.find cache (depth, String.of_list arrows) with
    | Some cached -> cached
    | None ->
        let num_key_presses =
          match depth with 0 -> List.length | _ -> loop ~depth:(depth - 1)
        in
        let res =
          keypad_paths ~pad:keypad arrows
          |> List.map ~f:(fun component ->
                 component
                 |> List.map ~f:(fun seq -> num_key_presses seq)
                 |> List.min_elt ~compare |> Option.value_exn)
          |> List.reduce_exn ~f:( + )
        in
        Hashtbl.Poly.set cache ~key:(depth, String.of_list arrows) ~data:res;
        res
  in
  loop

let instructions_len ~depth code =
  (*
     Leturn a minimal number of keypresses on the outermost keypad to enter a @code
     using a @depth levels of connected arrowkeyed-keypads
  *)
  let depth = depth - 1 in
  keypad_paths ~pad:numpad (String.to_list code)
  |> List.map ~f:(fun component ->
         component
         |> List.map ~f:(fun variant -> min_key_presses ~depth variant))
  |> List.map ~f:(fun lst -> List.min_elt ~compare lst |> Option.value_exn)
  |> List.reduce_exn ~f:( + )

let solve ~depth fname =
  In_channel.read_lines fname
  |> List.map ~f:(fun code ->
         let code_value = int_of_string (String.drop_suffix code 1) in
         code_value * instructions_len ~depth code)
  |> List.reduce_exn ~f:( + )

let () = assert (solve ~depth:2 "test/day21.txt" = 126384)
let () = assert (solve ~depth:2 "data/day21.txt" = 123096)
let () = assert (solve ~depth:25 "data/day21.txt" = 154517692795352)
