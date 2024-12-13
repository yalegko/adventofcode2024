open Core

type game = { buttonA : int * int; buttonB : int * int; target : int * int }

let extract_numbers equation_s =
  (*
     "Button X: X+94, Y+34" -> (94,34)
     "Prize: X=8400, Y=5400" -> (8400,5400)
  *)
  equation_s
  |> Re.exec (Re.compile (Re.Perl.re {|X.(\d+), Y.(\d+)|}))
  |> Re.Group.all |> Array.to_list |> List.tl_exn |> List.map ~f:int_of_string

let read_games fname =
  In_channel.read_all fname
  |> Re.split (Re.compile (Re.str "\n\n"))
  |> List.map ~f:(fun task ->
         String.split task ~on:'\n' |> List.map ~f:extract_numbers)
  |> List.map ~f:(fun task ->
         match task with
         | [ [ ax; ay ]; [ bx; by ]; [ tx; ty ] ] ->
             { buttonA = (ax, ay); buttonB = (bx, by); target = (tx, ty) }
         | _ -> failwith "Unexpected game format")

let solve game =
  let tx, ty = game.target in
  let (ax, ay), (bx, by) = (game.buttonA, game.buttonB) in
  match (ax * by) - (ay * bx) with
  | 0 -> None
  | det ->
      let x1 = (by * tx) - (bx * ty) and x2 = (-ay * tx) + (ax * ty) in
      if x1 mod det = 0 && x2 mod det = 0 then Some (x1 / det, x2 / det)
      else None

let cost (numA, numB) = (numA * 3) + numB

let solve1 fname =
  read_games fname |> List.map ~f:solve |> List.filter_map ~f:Fun.id
  |> List.filter ~f:(fun (x, y) -> x <= 100 && y <= 100)
  |> List.map ~f:cost |> List.reduce_exn ~f:( + )

let solve2 fname =
  read_games fname
  |> List.map ~f:(fun game ->
         let delta = 10000000000000 in
         {
           game with
           target = (fst game.target + delta, snd game.target + delta);
         })
  |> List.map ~f:solve |> List.filter_map ~f:Fun.id |> List.map ~f:cost
  |> List.reduce_exn ~f:( + )
