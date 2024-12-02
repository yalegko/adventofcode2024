
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let parse_lists file =
  read_lines file
  |> List.map (String.split_on_char ' ')
  |> List.map (List.filter (fun (v) -> String.length v != 0))
  |> List.fold_left 
      (fun [l1; l2] [v1 ; v2] -> 
        let i1 = int_of_string v1 in
        let i2 = int_of_string v2 in
        [i1::l1 ; i2::l2]
      
      )
      [[] ; []]
  |> List.map (List.sort compare)

let solve file =
  let [l1 ; l2] = parse_lists file in
  List.combine l1 l2
  |> List.map (fun (a, b) -> abs(a-b))
  |> List.fold_left (+) 0

let _ =
  assert ((solve "test/day01.txt") == 11);;

let _ =
  assert ((solve "data/day01.txt") == 2769675);;


let count_occurrences lst =
  lst
  |>List.fold_left
    (fun map x ->
      let count = try List.assoc x map with Not_found -> 0 in
      (x, count + 1) :: List.remove_assoc x map
    )
    []
    

let solve2 file =
  let [l1 ; l2] = parse_lists file in
  let freqs = count_occurrences l2 in
  l1
  |> List.map
    (fun (v) -> v * (try List.assoc v freqs with Not_found -> 0))
  |> List.fold_left (+) 0;;


let _ =
  assert ((solve2 "test/day01.txt") == 31);;

let _ =
  assert ((solve2 "data/day01.txt") == 24643097);;
  