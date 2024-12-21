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

let keypad_paths ~pathf to_press =
  list_to_pairs ('A' :: to_press)
  |> List.map ~f:(fun (from', to') -> pathf from' to')

let arrows_path2 p =
  keypad_paths
    ~pathf:(fun a b ->
      shortest_paths
        ~neighbors:(pad_neighbours ~field:keypad)
        ~start:(pad_coord ~pad:keypad a) ~finish:(pad_coord ~pad:keypad b))
    p
  |> List.map ~f:(fun lst -> List.map lst ~f:path_to_keys)

let shortest_path3 =
  let cache = Hashtbl.Poly.create () in
  let rec loop ~depth path =
    match Hashtbl.Poly.find cache (depth, String.of_list path) with
    | Some res -> res
    | None ->
        let lenf =
          match depth with 0 -> List.length | _ -> loop ~depth:(depth - 1)
        in

        let res =
          arrows_path2 path
          |> List.map ~f:(fun component ->
                 component
                 |> List.map ~f:(fun path -> lenf path)
                 |> List.min_elt ~compare |> Option.value_exn)
          |> List.reduce_exn ~f:( + )
        in
        Hashtbl.Poly.set cache ~key:(depth, String.of_list path) ~data:res;
        res
  in
  loop

let calc_string3 ~depth s =
  let parts =
    keypad_paths
      ~pathf:(fun a b ->
        shortest_paths
          ~neighbors:(pad_neighbours ~field:numpad)
          ~start:(pad_coord ~pad:numpad a) ~finish:(pad_coord ~pad:numpad b))
      (String.to_list s)
    |> List.map ~f:(fun lst -> List.map lst ~f:path_to_keys)
  in

  parts
  |> List.map ~f:(fun component ->
         component |> List.map ~f:(fun variant -> shortest_path3 ~depth variant))
  |> List.map ~f:(fun lst -> List.min_elt ~compare lst |> Option.value_exn)
  |> List.reduce_exn ~f:( + )

let solve ~depth fname =
  In_channel.read_lines fname
  |> List.map ~f:(fun code ->
         int_of_string (String.drop_suffix code 1) * calc_string3 ~depth code)
  |> List.reduce_exn ~f:( + )

let () = assert (solve ~depth:1 "test/day21.txt" = 126384)
let () = assert (solve ~depth:1 "data/day21.txt" = 123096)
let () = assert (solve ~depth:24 "data/day21.txt" = 154517692795352)
