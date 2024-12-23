open Core

let read_graph fname =
  let graph = Hashtbl.create (module String) in
  let add_connection from' to' =
    Hashtbl.update graph from' ~f:(function
      | None -> [ to' ]
      | Some old -> to' :: old)
  in
  In_channel.read_lines fname
  |> List.map ~f:(String.split ~on:'-')
  |> List.iter ~f:(function
       | [ from'; to' ] ->
           add_connection from' to';
           add_connection to' from'
       | _ -> failwith "incorrect input");
  graph

let find_cycles ~graph ~n start =
  let res = Hashtbl.create (module String) in

  let visited = Hashtbl.create (module String) in
  let rec dfs vert n path =
    let connections = Hashtbl.find_exn graph vert in
    if n = 0 then (
      if
        (* If connected to the start -- it's a loop *)
        List.find connections ~f:(String.equal start) |> is_some
      then
        Hashtbl.set res ~key:(String.concat ~sep:"-" (List.rev path)) ~data:())
    else (
      Hashtbl.set visited ~key:vert ~data:();

      connections
      |> List.filter ~f:(Fun.negate (Hashtbl.mem visited))
      |> List.iter ~f:(fun c -> dfs c (n - 1) (c :: path));

      Hashtbl.remove visited vert)
  in

  dfs start (n - 1) [ start ];
  Hashtbl.keys res

let solve1 fname =
  let graph = read_graph fname in

  Hashtbl.keys graph
  |> List.filter ~f:(String.is_prefix ~prefix:"t")
  |> List.concat_map ~f:(find_cycles ~graph ~n:3)
  |> List.map ~f:(fun cycle ->
         String.split cycle ~on:'-' |> List.sort ~compare:String.compare)
  |> List.dedup_and_sort ~compare:(fun a b -> List.compare String.compare a b)
  |> List.length

let () =
  assert (solve1 "test/day23.txt" = 7);
  assert (solve1 "data/day23.txt" = 1175)

let find_max_clique ~graph =
  let graph = Hashtbl.map graph ~f:String.Set.of_list in

  (* Looking for a vertex which neighbors have the biggest intersection w/ @candidates *)
  let find_pivot candidates excluded =
    Set.union candidates excluded
    |> Set.to_list
    |> List.map ~f:(fun v -> Set.inter candidates (Hashtbl.find_exn graph v))
    |> List.max_elt ~compare:(fun s1 s2 ->
           compare (Set.length s1) (Set.length s2))
    |> Option.bind ~f:Set.choose
    |> Option.bind ~f:(Hashtbl.find graph)
    |> Option.value ~default:String.Set.empty
  in

  (* Ref:
      Bron–Kerbosch algorithm with pivoting:
      - https://en.wikipedia.org/wiki/Bron–Kerbosch_algorithm
      - https://github.com/alanmc-zz/python-bors-kerbosch/blob/master/bors-kerbosch.py *)
  let max = ref String.Set.empty in
  let rec bron_kerb ~clique ~candidates ~excluded =
    if Set.is_empty candidates && Set.is_empty excluded then (
      if Set.length clique > Set.length !max then max := clique)
    else
      let _ =
        let pivot = find_pivot candidates excluded in

        Set.diff candidates pivot |> Set.to_list
        |> List.fold_left ~init:(candidates, excluded)
             ~f:(fun (candidates, excluded) v ->
               let connections = Hashtbl.find_exn graph v in

               bron_kerb ~clique:(Set.add clique v)
                 ~candidates:(Set.inter candidates connections)
                 ~excluded:(Set.inter excluded connections);

               (Set.remove candidates v, Set.add excluded v))
      in
      ()
  in

  bron_kerb ~clique:String.Set.empty
    ~candidates:(String.Set.of_list (Hashtbl.keys graph))
    ~excluded:String.Set.empty;

  Set.to_list !max |> List.sort ~compare:String.compare

let solve2 fname =
  let graph = read_graph fname in
  find_max_clique ~graph |> String.concat ~sep:","

let () =
  assert (String.equal "co,de,ka,ta" (solve2 "test/day23.txt"));
  assert (
    String.equal "bw,dr,du,ha,mm,ov,pj,qh,tz,uv,vq,wq,xw"
      (solve2 "data/day23.txt"))
