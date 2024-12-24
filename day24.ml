open Core

type gate = Const of int | Logical of (string * string * string)
[@@deriving sexp, compare]

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

let gate_key (g1, op, g2) =
  List.sort ~compare:String.compare [ g1; op; g2 ] |> String.concat ~sep:" "

let rec expect ~scheme ~gates z ((a, op, b) as expgate) =
  let get_logical gate =
    match Hashtbl.find_exn scheme gate with
    | Logical (g1, op, g2) -> (g1, op, g2)
    | _ -> failwith "unreachable"
  in

  let ((zl, zop, zr) as zgate) = get_logical z in

  if String.equal (gate_key zgate) (gate_key expgate) then None
  else
    match Hashtbl.find gates (gate_key expgate) with
    | Some actual ->
        printf "Swap %s -> %s!\n" z actual;
        printf "\t %s = %s %s %s\n" z zl zop zr;
        printf "\t %s = %s %s %s\n" actual a op b;

        Hashtbl.set gates ~key:(gate_key zgate) ~data:actual;
        Hashtbl.set gates ~key:(gate_key expgate) ~data:z;
        Hashtbl.set scheme ~key:z ~data:(Logical expgate);
        Hashtbl.set scheme ~key:actual ~data:(Logical zgate);

        Some (z, actual)
    | None ->
        if String.equal zl a then expect ~scheme ~gates zr (get_logical b)
        else if String.equal zl b then expect ~scheme ~gates zr (get_logical a)
        else if String.equal zr a then expect ~scheme ~gates zl (get_logical b)
        else if String.equal zr b then expect ~scheme ~gates zl (get_logical a)
        else failwith "unreachable"

let _ =
  let scheme = read_scheme "data/day24.txt" in

  let gates = String.Table.create () in

  Hashtbl.to_alist scheme
  |> List.iter ~f:(fun (name, wire) ->
         match wire with
         | Const _ -> ()
         | Logical l -> Hashtbl.add_exn gates ~key:(gate_key l) ~data:name);

  let name_of logical =
    match Hashtbl.find gates (gate_key logical) with
    | Some v -> v
    | None -> failwithf "can't find %s" (gate_key logical) ()
  in

  let res = ref [] in
  let carry =
    List.range 0 45
    |> List.fold_left ~init:None ~f:(fun carry i ->
           let x = sprintf "x%02d" i
           and y = sprintf "y%02d" i
           and z = sprintf "z%02d" i in

           match carry with
           | None ->
               let some_swap = expect ~scheme ~gates z (x, "XOR", y) in
               (match some_swap with
               | Some (s1, s2) -> res := s1 :: s2 :: !res
               | _ -> ());
               Some (x, "AND", y)
           | Some carry ->
               (* zi = xi ^ yi ^ carry *)
               let some_swap =
                 expect ~scheme ~gates z
                   (name_of (x, "XOR", y), "XOR", name_of carry)
               in
               (match some_swap with
               | Some (s1, s2) -> res := s1 :: s2 :: !res
               | _ -> ());

               (* carry = (xi & yi) | (xi ^ yi) & carry *)
               Some
                 ( name_of (x, "AND", y),
                   "OR",
                   name_of (name_of (x, "XOR", y), "AND", name_of carry) ))
  in

  List.sort !res ~compare:String.compare |> String.concat ~sep:","
