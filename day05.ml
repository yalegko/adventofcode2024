open Core

let parse_rules rules_s =
  String.split_lines rules_s
  |> List.map ~f:(String.split ~on:'|')
  |> List.map ~f:(fun pair ->
         match pair with
         | [ x; y ] -> (x, y)
         | _ -> raise (Failure "Unexpected input"))
  |> String.Table.of_alist_multi

let parse_pages pages_s =
  String.split_lines pages_s |> List.map ~f:(String.split ~on:',')

let parse filename =
  let all = In_channel.read_all filename in
  let idx = String.substr_index_exn all ~pattern:"\n\n" in
  let rules = String.slice all 0 idx and pages = String.slice all (idx + 2) 0 in
  (parse_rules rules, parse_pages pages)

let is_ordered rules list =
  list
  |> List.mapi ~f:(fun i v ->
         Set.is_subset
           (String.Set.of_list (List.drop list (i + 1)))
           ~of_:
             (String.Set.of_list
                (Option.value ~default:[] (Hashtbl.find rules v))))
  |> List.fold ~init:true ~f:( && )

let () =
  let rules, _ = parse "test/day05.txt" in
  assert (is_ordered rules (List.hd_exn (parse_pages "75,47,61,53,29")));
  assert (not (is_ordered rules (List.hd_exn (parse_pages "75,97,47,61,53"))))

let solve1 filename =
  let rules, pages = parse filename in
  pages
  |> List.filter ~f:(is_ordered rules)
  |> List.map ~f:(fun l -> List.nth_exn l (List.length l / 2))
  |> List.map ~f:int_of_string |> List.fold ~init:0 ~f:( + )

let () =
  assert (solve1 "test/day05.txt" = 143);
  assert (solve1 "data/day05.txt" = 5208)

let partial_compare rules x y =
  let after a = Option.value ~default:[] (Hashtbl.find rules a) in
  let ( |>> ) a b =
    after b |> List.find ~f:(String.equal a) |> Option.is_some
  in
  if x |>> y then 1 else if y |>> x then -1 else 0

let solve2 filename =
  let rules, pages = parse filename in
  pages
  |> List.filter ~f:(Fun.negate (is_ordered rules))
  |> List.map ~f:(List.sort ~compare:(partial_compare rules))
  |> List.map ~f:(fun l -> List.nth_exn l (List.length l / 2))
  |> List.map ~f:int_of_string |> List.fold ~init:0 ~f:( + )

let () =
  assert (solve2 "test/day05.txt" = 123);
  assert (solve2 "data/day05.txt" = 6732)
