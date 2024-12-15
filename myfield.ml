open Core

module Point = struct
  type t = int * int [@@deriving compare, sexp, hash, equal]

  let to_string p = p |> sexp_of_t |> Sexp.to_string_hum
  let neighbors (x, y) = [ (x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1) ]
end

module PointSet = Set.Make (Point)

let of_string s =
  s |> String.split_lines |> List.map ~f:String.to_array |> List.to_array

let read file = In_channel.read_all file |> of_string
let get field (i, j) = try Some field.(i).(j) with Invalid_argument _ -> None
let contains field p = get field p |> is_some

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
