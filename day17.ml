open Core

type 'a vm = { rax : 'a; rbx : 'a; rcx : 'a; rip : int }
type opcode = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV

let opcode_of_byte b =
  match b with
  | 0 -> ADV
  | 1 -> BXL
  | 2 -> BST
  | 3 -> JNZ
  | 4 -> BXC
  | 5 -> OUT
  | 6 -> BDV
  | 7 -> CDV
  | _ -> failwithf "Unknown opcode byte: %d" b ()

let cut s ~on =
  match s |> Re.split (Re.compile (Re.str on)) with
  | [ prefix; suffix ] -> (prefix, suffix)
  | _ -> failwith "Failed to parse register value"

let read_device fname =
  let fname = fname in
  let registers, opcodes =
    In_channel.read_all fname |> Re.split (Re.compile (Re.str "\n\n"))
    |> function
    | [ f; m ] -> (f, m)
    | _ -> failwith "Unexpected input"
  in
  let registers =
    String.split_lines registers
    |> List.map ~f:(fun reg -> cut reg ~on:": " |> snd |> int_of_string)
    |> Array.of_list
  in
  let program =
    cut opcodes ~on:": " |> snd |> String.split ~on:','
    |> List.map ~f:int_of_string |> Array.of_list
  in
  ( { rax = registers.(0); rbx = registers.(1); rcx = registers.(2); rip = 0 },
    program )

let combo vm op =
  match op with
  | v when v <= 3 -> v
  | 4 -> vm.rax
  | 5 -> vm.rbx
  | 6 -> vm.rcx
  | _ -> failwithf "invalid combo op %d" op ()

let exec vm opcode op =
  match opcode_of_byte opcode with
  | ADV -> ({ vm with rax = vm.rax / Int.pow 2 (combo vm op) }, [])
  | BXL -> ({ vm with rbx = Int.bit_xor vm.rbx op }, [])
  | BST -> ({ vm with rbx = combo vm op mod 8 }, [])
  | JNZ -> ({ vm with rip = (if vm.rax <> 0 then op else vm.rip) }, [])
  | BXC -> ({ vm with rbx = Int.bit_xor vm.rbx vm.rcx }, [])
  | OUT -> (vm, [ combo vm op mod 8 ])
  | BDV -> ({ vm with rbx = vm.rax / Int.pow 2 (combo vm op) }, [])
  | CDV -> ({ vm with rcx = vm.rax / Int.pow 2 (combo vm op) }, [])

let read_op program rip =
  try Some (program.(rip), program.(rip + 1)) with _ -> None

let run vm program =
  let stdout = ref [] in
  let rec loop vm =
    match read_op program vm.rip with
    | None -> !stdout
    | Some (opcode, op) ->
        let new_state, out = exec vm opcode op in
        stdout := !stdout @ out;
        loop
          {
            new_state with
            rip =
              (if new_state.rip <> vm.rip then new_state.rip else vm.rip + 2);
          }
  in
  loop vm

let solve1 fname =
  let vm, program = read_device fname in
  run vm program |> List.map ~f:string_of_int |> String.concat ~sep:","

let () =
  assert (String.equal (solve1 "test/day17.txt") "4,6,3,5,6,3,5,2,1,0");
  assert (String.equal (solve1 "data/day17.txt") "1,5,0,3,7,3,0,3,1")

let make_bv ctx v = Z3.BitVector.mk_numeral ctx (string_of_int v) 64

let symbol_exec ctx vm opcode op expected =
  let make_bv v = make_bv ctx v in
  let xor a b = Z3.BitVector.mk_xor ctx a b in
  let shr a b = Z3.BitVector.mk_lshr ctx a b in
  let mod8 bv = Z3.BitVector.mk_and ctx bv (make_bv 7) in
  let eq a b = Z3.Boolean.mk_eq ctx a b in
  let neq a b = Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx a b) in

  let symbol_combo op =
    match op with
    | v when v <= 3 -> make_bv v
    | 4 -> vm.rax
    | 5 -> vm.rbx
    | 6 -> vm.rcx
    | _ -> failwithf "invalid combo op %d" op ()
  in

  match opcode_of_byte opcode with
  | ADV -> ({ vm with rax = shr vm.rax (symbol_combo op) }, [])
  | BXL -> ({ vm with rbx = xor vm.rbx (make_bv op) }, [])
  | BST -> ({ vm with rbx = mod8 (symbol_combo op) }, [])
  | JNZ ->
      ( vm,
        match expected with
        | None -> [ eq vm.rax (make_bv 0) ]
        | Some _ -> [ neq vm.rax (make_bv 0) ] )
  | BXC -> ({ vm with rbx = xor vm.rbx vm.rcx }, [])
  | OUT ->
      ( vm,
        match expected with
        | None -> failwith "unreachable"
        | Some v -> [ eq (mod8 (symbol_combo op)) (make_bv v) ] )
  | BDV -> ({ vm with rbx = shr vm.rax (symbol_combo op) }, [])
  | CDV -> ({ vm with rcx = shr vm.rax (symbol_combo op) }, [])

let solve2 fname =
  let vm, program = read_device fname in

  let ctx = Z3.mk_context [ ("model", "true"); ("proof", "false") ] in
  let rax = Z3.BitVector.mk_const ctx (Z3.Symbol.mk_string ctx "rax") 64 in
  let vm =
    { rax; rbx = make_bv ctx vm.rbx; rcx = make_bv ctx vm.rcx; rip = 0 }
  in

  let solver = Z3.Optimize.mk_opt ctx in
  let _vm =
    Array.mapi program ~f:(fun i c -> (i, c))
    |> Array.fold ~init:vm ~f:(fun vm (i, c) ->
           let vm = { vm with rip = 0 } in
           let is_last_iter = Array.length program - 1 = i in
           let rec loop vm =
             match read_op program vm.rip with
             | None -> vm
             | Some (opcode, op) ->
                 let expected =
                   if opcode = 3 && is_last_iter then None else Some c
                 in
                 let vm, constraints = symbol_exec ctx vm opcode op expected in
                 Z3.Optimize.add solver constraints;
                 loop { vm with rip = vm.rip + 2 }
           in
           loop vm)
  in

  let _ = Z3.Optimize.minimize solver rax in

  match Z3.Optimize.check solver with
  | UNKNOWN -> failwith "unknown"
  | UNSATISFIABLE -> failwith "unsat"
  | SATISFIABLE -> (
      match Z3.Optimize.get_model solver with
      | None -> failwith "no model?"
      | Some model -> (
          match Z3.Model.get_const_interp_e model rax with
          | None -> failwith "no rax?"
          | Some v -> Z3.BitVector.numeral_to_string v |> int_of_string))

let () =
  assert (solve2 "test/day17-2.txt" = 117440);
  assert (solve2 "data/day17.txt" = 105981155568026)
