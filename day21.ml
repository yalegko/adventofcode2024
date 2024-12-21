open Core

let numpad =
  [|
    [| '7'; '8'; '9' |];
    [| '4'; '5'; '6' |];
    [| '1'; '2'; '3' |];
    [| ' '; '0'; 'A' |];
  |]

let keypad = [| [| ' '; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]

let pad_neighbours ~field pos =
  Myfield.Point.neighbors pos
  |> List.filter ~f:(Myfield.contains field)
  |> List.filter ~f:(fun n -> not (Myfield.eq_at ~field ~c:' ' n))

let shortest_paths ~neighbors ~start ~finish =
  let paths = ref [] in
  let shortest = ref 100500 in

  let visited = Hashtbl.create (module Myfield.Point) in
  let rec dfs path =
    match path with
    | [] -> failwith "unreachable"
    | pos :: _tail when Hashtbl.mem visited pos -> ()
    | pos :: _tail when Myfield.Point.equal pos finish -> (
        match List.length path with
        | l when l < !shortest ->
            shortest := List.length path;
            paths := [ path ]
        | l when l = !shortest -> paths := path :: !paths
        | _ -> ())
    | pos :: _tail ->
        Hashtbl.set visited ~key:pos ~data:();
        List.iter (neighbors pos) ~f:(fun n -> dfs (n :: path));
        Hashtbl.remove visited pos
  in
  dfs [ start ];
  List.map !paths ~f:List.rev

let list_to_pairs lst =
  List.fold_left (List.drop lst 1)
    ~init:([], List.hd_exn lst)
    ~f:(fun (res, prev) v -> ((prev, v) :: res, v))
  |> fst |> List.rev

let path_to_keys path =
  list_to_pairs path
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

let rec cartesian_product = function
  | [] -> [ [] ]
  | lst :: tail ->
      let tail_product = cartesian_product tail in
      List.concat_map lst ~f:(fun x ->
          List.map tail_product ~f:(fun variant -> x :: variant))

let cached_shortest_paths ~pad =
  print_endline "Recreating cache";
  let memo = Hashtbl.create (module String) in
  let impl from' to' =
    let memo_key = String.of_list [ from'; to' ] in
    match Hashtbl.find memo memo_key with
    | Some res -> res
    | None ->
        let res =
          shortest_paths
            ~neighbors:(pad_neighbours ~field:pad)
            ~start:(pad_coord ~pad from') ~finish:(pad_coord ~pad to')
          |> List.map ~f:path_to_keys
        in
        Hashtbl.set memo ~key:memo_key ~data:res;
        print_endline (sprintf "Cached %d" (Hashtbl.length memo));
        res
  in
  impl

let shortest_numpad_paths = cached_shortest_paths ~pad:numpad
let shortest_keypad_paths = cached_shortest_paths ~pad:keypad

let keypad_paths ~pathf to_press =
  list_to_pairs ('A' :: to_press)
  |> List.map ~f:(fun (from', to') -> pathf from' to')
  |> cartesian_product
  |> List.map ~f:(fun path -> path |> List.concat)

let code_paths code =
  keypad_paths ~pathf:shortest_numpad_paths ('A' :: String.to_list code)

let arrows_path path = keypad_paths ~pathf:shortest_keypad_paths ('A' :: path)

let select_shortest paths =
  List.fold_left paths ~init:(100500, [])
    ~f:(fun (shortest, shortest_paths) path ->
      match List.length path with
      | l when l < shortest -> (l, [ path ])
      | l when l = shortest -> (shortest, path :: shortest_paths)
      | _ -> (shortest, shortest_paths))
  |> snd

let shortest_code_len code =
  code_paths code
  |> List.concat_map ~f:arrows_path
  |> select_shortest
  |> List.concat_map ~f:arrows_path
  |> List.map ~f:List.length |> List.min_elt ~compare |> Option.value_exn

let solve1 fname =
  let codes = In_channel.read_lines fname in
  List.map codes ~f:(fun code -> (code, shortest_code_len code))
  |> List.map ~f:(fun (code, len) ->
         int_of_string (String.drop_suffix code 1) * len)
  |> List.reduce_exn ~f:( + )

let () = assert (Util.time solve1 "data/day21.txt" = 132453)
