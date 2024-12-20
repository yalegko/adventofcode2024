open Core

module Point = struct
  type t = int * int [@@deriving compare, sexp, hash, equal]

  let to_string p = p |> sexp_of_t |> Sexp.to_string_hum
  let neighbors (x, y) = [ (x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1) ]
end

module PointWithDir = struct
  type t = (int * int) * char [@@deriving compare, sexp, hash, equal]

  let to_string p = p |> sexp_of_t |> Sexp.to_string_hum
end

module PointSet = Set.Make (Point)

let of_string s =
  s |> String.split_lines |> List.map ~f:String.to_array |> List.to_array

let read file = In_channel.read_all file |> of_string
let get field (i, j) = try Some field.(i).(j) with Invalid_argument _ -> None
let contains field p = get field p |> is_some
let eq_at ~field ~c p = get field p |> Option.exists ~f:(Char.equal c)

let find field ~f =
  Array.mapi field ~f:(fun i row ->
      Array.filter_mapi row ~f:(fun j elem ->
          if f elem then Some (i, j) else None))
  |> Array.map ~f:List.of_array |> List.of_array |> List.concat

let fold field ~init ~f =
  Array.foldi field ~init ~f:(fun i acc row ->
      Array.foldi row ~init:acc ~f:(fun j acc _el -> f acc (i, j)))

let print field =
  Array.iter field ~f:(fun row -> print_endline (String.of_array row))

let do_step ~direction (x, y) =
  match direction with
  | '^' -> (x - 1, y)
  | '>' -> (x, y + 1)
  | 'v' -> (x + 1, y)
  | '<' -> (x, y - 1)
  | dir -> failwithf "Unexpected direction %c" dir ()

let calc_distances field ~start =
  let distances = Hashtbl.create (module Point) in
  Hashtbl.set distances ~key:start ~data:0;

  let visited = Hashtbl.create (module Point) in
  let cmp_dist (_pos1, dist1) (_pos2, dist2) = compare dist1 dist2 in
  let queue = Pairing_heap.of_list ~cmp:cmp_dist [ (start, 0) ] in
  let rec dijkstra () =
    match Pairing_heap.pop queue with
    | None -> ()
    | Some (pos, _dist) when Hashtbl.mem visited pos -> dijkstra ()
    | Some (pos, dist) ->
        Hashtbl.set visited ~key:pos ~data:();

        let perspective_neighbors =
          Point.neighbors pos
          |> List.filter ~f:(contains field)
          |> List.filter ~f:(fun n -> not (eq_at ~field ~c:'#' n))
          |> List.filter ~f:(fun n ->
                 match Hashtbl.find distances n with
                 | None -> true
                 | Some d -> dist + 1 < d)
        in

        List.iter perspective_neighbors ~f:(fun n ->
            Hashtbl.set distances ~key:n ~data:(dist + 1);
            Pairing_heap.add queue (n, dist + 1));

        dijkstra ()
  in
  dijkstra ();
  distances
