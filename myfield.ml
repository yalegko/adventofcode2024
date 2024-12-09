open Core

module Point = struct
  type t = int * int [@@deriving compare, sexp]

  let to_string p = p |> sexp_of_t |> Sexp.to_string_hum
end

let read file =
  In_channel.read_lines file |> List.map ~f:String.to_array |> List.to_array

let get field (i, j) = try Some field.(i).(j) with Invalid_argument _ -> None

let contains field p = get field p |> is_some

let find field ~f = 
  Array.mapi field ~f:(fun i row ->
    Array.filter_mapi row ~f:(fun j elem ->
        if f elem then Some (i, j) else None))
|> Array.map ~f:List.of_array |> List.of_array |> List.concat