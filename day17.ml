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

class symbolicVM ctx a b c =
  object (self)
    val ctx = ctx
    val mutable rax = a
    val mutable rbx = b
    val mutable rcx = c
    method get_rax = rax

    (* Arithmetic helpers *)
    method eq a b = Z3.Boolean.mk_eq ctx a b
    method neq a b = Z3.Boolean.mk_not ctx (self#eq a b)
    method xor a b = Z3.BitVector.mk_xor ctx a b
    method shr a b = Z3.BitVector.mk_lshr ctx a b
    method mod8 bv = Z3.BitVector.mk_and ctx bv (self#make_bv 7)
    method make_bv v = make_bv ctx v

    method combo op =
      match op with
      | v when v <= 3 -> self#make_bv v
      | 4 -> rax
      | 5 -> rbx
      | 6 -> rcx
      | _ -> failwithf "invalid combo op %d" op ()

    method _exec opcode op =
      match opcode_of_byte opcode with
      | ADV -> rax <- self#shr rax (self#combo op)
      | BXL -> rbx <- self#xor rbx (self#make_bv op)
      | BST -> rbx <- self#mod8 (self#combo op)
      | BXC -> rbx <- self#xor rbx rcx
      | BDV -> rbx <- self#shr rax (self#combo op)
      | CDV -> rcx <- self#shr rax (self#combo op)
      | JNZ -> ()
      | OUT -> ()

    method constraints opcode op ~expected =
      match opcode_of_byte opcode with
      (* Do not finish right now -- perform a jump *)
      | JNZ -> [ self#neq rax (self#make_bv 0) ]
      (* Output the symbol we expecting *)
      | OUT -> [ self#eq (self#mod8 (self#combo op)) (self#make_bv expected) ]
      | _ -> []

    (* We solve it with 2 assumptions:
        1) We output only one symbol per run
        2) We have only jump at the end of the program, so we can always safely do RIP+=2
    *)
    method execute program expected =
      let rec loop rip constraints =
        match read_op program rip with
        | None -> constraints
        | Some (opcode, op) ->
            self#_exec opcode op;
            let new_constraints = self#constraints opcode op ~expected in
            loop (rip + 2) constraints @ new_constraints
      in
      loop 0 []
  end

let solve2 fname =
  let vm, program = read_device fname in

  (* Ensure we have only 1 jump at the end of the program *)
  let is_correct =
    let rec loop rip opcodes =
      match opcodes with
      | opcode :: op :: _tail when opcode = 3 ->
          op = 0 && rip = Array.length program - 2
      | _ :: _ :: tail -> loop (rip + 2) tail
      | _ -> failwith "unbalanced program"
    in
    loop 0 (List.of_array program)
  in
  assert is_correct;

  (* Create z3 context *)
  let ctx = Z3.mk_context [ ("model", "true"); ("proof", "false") ] in
  let solver = Z3.Optimize.mk_opt ctx in

  (* Prepare the symbolic VM. RAX is a variable, other regs are values. *)
  let rax = Z3.BitVector.mk_const ctx (Z3.Symbol.mk_string ctx "rax") 64 in
  let vm = new symbolicVM ctx rax (make_bv ctx vm.rbx) (make_bv ctx vm.rcx) in

  (* We iterate through whe expected output (our program) to collect expectations in the solver *)
  (* 1) Do all but last and expect EAX to be != 0 to take a jump back *)
  let program_but_last = Array.slice program 0 (Array.length program - 1) in
  Array.iter program_but_last ~f:(fun expected ->
      let constraints = vm#execute program expected in
      Z3.Optimize.add solver constraints);

  (* 2) Do the last iteration and do not expect the jump *)
  Z3.Optimize.add solver (vm#execute program_but_last (Array.last program));
  Z3.Optimize.add solver [ Z3.Boolean.mk_eq ctx vm#get_rax (make_bv ctx 0) ];

  (* Ask z3 to minimize RAX value *)
  let _ = Z3.Optimize.minimize solver rax in

  (* Extract the result from symbolic model *)
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
