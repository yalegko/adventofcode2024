open Core

type gate = Const of int | Logical of (string * string * string)

let cut s ~sep =
  Re.split (Re.compile (Re.str sep)) s |> function
  | [ p1; p2 ] -> (p1, p2)
  | _ -> failwith "Unexpected input"

let read_scheme fname =
  let inputs, gates =
    In_channel.read_all fname |> cut ~sep:"\n\n"
    |> Tuple2.map ~f:String.split_lines
  in

  let scheme = String.Table.create () in
  List.iter inputs ~f:(fun line ->
      let name, v = cut line ~sep:": " in
      Hashtbl.add_exn scheme ~key:name ~data:(Const (int_of_string v)));
  List.iter gates ~f:(fun line ->
      let logic, name = cut line ~sep:" -> " in
      match String.split ~on:' ' logic with
      | [ g1; op; g2 ] ->
          Hashtbl.add_exn scheme ~key:name ~data:(Logical (g1, op, g2))
      | _ -> failwith "Unexpected gate format");
  scheme

let rec eval_gate ~scheme gate =
  match Hashtbl.find_exn scheme gate with
  | Const c -> c
  | Logical (g1, op, g2) ->
      let g1v = eval_gate ~scheme g1 and g2v = eval_gate ~scheme g2 in
      let res =
        match op with
        | "OR" -> g1v lor g2v
        | "AND" -> g1v land g2v
        | "XOR" -> g1v lxor g2v
        | _ -> failwithf "unknown operation %s" op ()
      in
      Hashtbl.set scheme ~key:gate ~data:(Const res);
      res

let evaluate scheme =
  let outputs =
    Hashtbl.keys scheme
    |> List.filter ~f:(String.is_prefix ~prefix:"z")
    |> List.sort ~compare:String.compare
    |> List.rev
  in

  let scheme = Hashtbl.copy scheme in
  List.map outputs ~f:(eval_gate ~scheme)
  |> List.fold_left ~init:0 ~f:(fun res b -> (res lsl 1) lor b)

let solve1 fname = read_scheme fname |> evaluate

let () =
  assert (solve1 "test/day24.txt" = 4);
  assert (solve1 "data/day24.txt" = 46463754151024)

let normalize_logic (g1, op, g2) =
  let g1, g2 = if String.compare g1 g2 < 0 then (g1, g2) else (g2, g1) in
  (g1, op, g2)

let gate_key l =
  let g1, op, g2 = normalize_logic l in
  g1 ^ " " ^ op ^ " " ^ g2

let rec expect ~scheme ~gates name expected_logic =
  let get_logical gate_name =
    match Hashtbl.find_exn scheme gate_name with
    | Logical (g1, op, g2) -> (g1, op, g2)
    | _ -> failwith "unreachable"
  in

  let exp1, eop, exp2 = expected_logic |> normalize_logic in
  let ((gate1, op, gate2) as gate_logic) =
    get_logical name |> normalize_logic
  in

  if String.equal (gate_key gate_logic) (gate_key expected_logic) then
    (* Logic connected correctly -- proceed *)
    None
  else
    match Hashtbl.find gates (gate_key expected_logic) with
    (* Already have expected logic under different key -- found a swap *)
    | Some actual_name ->
        Hashtbl.set gates ~key:(gate_key gate_logic) ~data:actual_name;
        Hashtbl.set gates ~key:(gate_key expected_logic) ~data:name;
        Hashtbl.set scheme ~key:name ~data:(Logical expected_logic);
        Hashtbl.set scheme ~key:actual_name ~data:(Logical gate_logic);
        Some (name, actual_name)
    (* Can't find expected logic in scheme, look in operands *)
    | None ->
        assert (String.equal eop op);
        if String.equal gate1 exp1 then
          expect ~scheme ~gates gate2 (get_logical exp2)
        else if String.equal gate2 exp2 then
          expect ~scheme ~gates gate1 (get_logical exp1)
        else failwith "unreachable"

let solve2 fname =
  (* Scheme maps gate name to logic *)
  let scheme =
    read_scheme fname
    |> Hashtbl.map ~f:(function
         | Logical l -> Logical (normalize_logic l)
         | const -> const)
  in

  (* Gates maps gate logic to name *)
  let gates =
    Hashtbl.to_alist scheme
    |> List.filter_map ~f:(fun (name, wire) ->
           match wire with
           | Const _ -> None
           | Logical l -> Some (gate_key l, name))
    |> String.Table.of_alist_exn
  in
  let name_of logical =
    match Hashtbl.find gates (gate_key logical) with
    | Some v -> v
    | None -> failwithf "can't find %s" (gate_key logical) ()
  in

  (* Too tired to collect result functionally *)
  let res = ref [] in
  let mb_add_to_res some_swap =
    match some_swap with Some (s1, s2) -> res := s1 :: s2 :: !res | _ -> ()
  in

  (* Here we implement a summator logic like x + y = z:

         z0 = x0 ^ y0
         carry = x0 & y1

         zi = xi ^ yi ^ carry
         carry = xi & yi | (xi ^ yi) & carry

         zn = carry

     Ref: https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder
  *)
  expect ~scheme ~gates "z00" ("x00", "XOR", "y00") |> mb_add_to_res;
  let carry =
    List.range 1 45
    |> List.map ~f:(fun i ->
           let x = sprintf "x%02d" i
           and y = sprintf "y%02d" i
           and z = sprintf "z%02d" i in
           (x, y, z))
    |> List.fold_left ~init:("x00", "AND", "y00") ~f:(fun carry (xi, yi, zi) ->
           (* zi = xi ^ yi ^ carry *)
           expect ~scheme ~gates zi
             (name_of (xi, "XOR", yi), "XOR", name_of carry)
           |> mb_add_to_res;

           (* carry = (xi & yi) | (xi ^ yi) & carry *)
           ( name_of (xi, "AND", yi),
             "OR",
             name_of (name_of (xi, "XOR", yi), "AND", name_of carry) ))
  in
  expect ~scheme ~gates "z45" carry |> mb_add_to_res;

  List.sort !res ~compare:String.compare |> String.concat ~sep:","

let () =
  assert (
    String.equal (solve2 "data/day24.txt") "cqk,fph,gds,jrs,wrk,z15,z21,z34")
