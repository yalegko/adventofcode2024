open Core

let read_game fname =
  let field, moves =
    In_channel.read_all fname |> Re.split (Re.compile (Re.str "\n\n"))
    |> function
    | [ f; m ] -> (f, m)
    | _ -> failwith "Unexpected input"
  in
  let field =
    String.split field ~on:'\n' |> Array.of_list |> Array.map ~f:String.to_array
  in
  let moves =
    String.to_list moves |> List.filter ~f:(Fun.negate (Char.equal '\n'))
  in
  (field, moves)

let find_start field = Myfield.find field ~f:(Char.equal '@') |> List.hd_exn

let do_step ~direction (x, y) =
  match direction with
  | '^' -> (x - 1, y)
  | '>' -> (x, y + 1)
  | 'v' -> (x + 1, y)
  | '<' -> (x, y - 1)
  | dir -> failwith ("Unexpected direction " ^ Char.to_string dir)

let copy field = Array.copy field |> Array.map ~f:Array.copy

let print_with_robot field pos =
  let f = copy field in
  f.(fst pos).(snd pos) <- '@';
  Myfield.print f

let debug = false

let simulate ~is_box ~push_boxes ~field moves =
  let start = find_start field in
  field.(fst start).(snd start) <- '.';

  let rec do_steps pos moves =
    match moves with
    | [] -> field
    | hd :: tail -> (
        if debug then (
          let _ = In_channel.(input_line_exn stdin) in
          printf "%c (rest %d)\n" hd (List.length tail);
          print_with_robot field pos;
          print_endline "");

        let next = do_step ~direction:hd pos in
        match field.(fst next).(snd next) with
        | '#' -> do_steps pos tail
        | '.' -> do_steps next tail
        | b when is_box b ->
            let moved = push_boxes ~direction:hd pos in
            let pos = if moved then next else pos in
            do_steps pos tail
        | block -> failwith ("Unexpected field block " ^ Char.to_string block))
  in
  do_steps start moves

let simulate1 field moves =
  let rec push ~direction box_pos =
    let next = do_step ~direction box_pos in
    let move ~from:(x, y) ~to':(x', y') =
      field.(x).(y) <- '.';
      field.(x').(y') <- 'O'
    in
    match field.(fst next).(snd next) with
    | '#' -> false
    | '.' ->
        move ~from:box_pos ~to':next;
        true
    | 'O' ->
        let moved = push ~direction next in
        if moved then move ~from:box_pos ~to':next;
        moved
    | block -> failwith ("Unexpected field block " ^ Char.to_string block)
  in
  let push_boxes ~direction p = push ~direction (do_step ~direction p) in
  simulate ~is_box:(Char.equal 'O') ~push_boxes ~field moves

let solve1 fname =
  let field, moves = read_game fname in
  simulate1 field moves
  |> Myfield.find ~f:(Char.equal 'O')
  |> List.map ~f:(fun (x, y) -> (x * 100) + y)
  |> List.reduce_exn ~f:( + )

let () =
  assert (solve1 "test/day15.txt" = 10092);
  assert (solve1 "data/day15.txt" = 1505963)

let inflate field =
  Array.map field ~f:(fun row ->
      Array.concat_map row ~f:(fun el ->
          String.to_array
            (match el with
            | '#' -> "##"
            | '.' -> ".."
            | '@' -> "@."
            | 'O' -> "[]"
            | block ->
                failwith ("Unexpected field block " ^ Char.to_string block))))

let simulate_wide_box field moves =
  let field = inflate field in
  let get (x, y) = field.(x).(y) in
  let move ~from:(x, y) ~to':(x', y') =
    field.(x').(y') <- field.(x).(y);
    field.(x).(y) <- '.'
  in

  let is_box ch = Char.equal ch '[' || Char.equal ch ']' in
  let part2 (x, y) =
    match get (x, y) with
    | '[' -> (x, y + 1)
    | ']' -> (x, y - 1)
    | _other -> failwithf "Not a box part %c" _other ()
  in

  let is_vertical direction =
    match direction with
    | '>' | '<' -> false
    | 'v' | '^' -> true
    | dir -> failwithf "Unexpected direction %c" dir ()
  in

  let rec can_push ~direction pos =
    match get pos with
    | '.' -> true
    | '#' -> false
    | '[' | ']' ->
        let can_push_this = can_push ~direction (do_step ~direction pos) in
        if is_vertical direction then
          can_push_this && can_push ~direction (do_step ~direction (part2 pos))
        else can_push_this
    | block -> failwithf "Unexpected field block %c" block ()
  in

  let rec push ~direction pos =
    let next = do_step ~direction pos in
    match get next with
    | '.' -> move ~from:pos ~to':next
    | '[' | ']' ->
        if is_vertical direction then push ~direction (part2 next);
        push ~direction next;
        move ~from:pos ~to':next
    | _wall -> ()
  in

  let push_boxes ~direction robo_pos =
    let next = do_step ~direction robo_pos in
    let pushed = can_push ~direction next in
    if pushed then push ~direction robo_pos;
    pushed
  in
  simulate ~is_box ~push_boxes ~field moves

let solve2 fname =
  let field, moves = read_game fname in
  simulate_wide_box field moves
  |> Myfield.find ~f:(Char.equal '[')
  |> List.map ~f:(fun (x, y) -> (x * 100) + y)
  |> List.reduce_exn ~f:( + )

let () =
  assert (solve2 "test/day15.txt" = 9021);
  assert (solve2 "data/day15.txt" = 1543141)
