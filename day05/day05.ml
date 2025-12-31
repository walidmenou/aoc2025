open Aoc2025.Parser;;

let range =
  number <<| char '-'
  |*> fun a -> number
  |*> fun b -> of_value (a, b)

let ranges = sep_by newline range
let ids = sep_by newline number

let parse_input =
  ranges
  |*> fun ranges -> (newline |>> newline |>> ids)
  |*> fun ids -> of_value (ranges, ids)
;;

let rec valid_id id ranges =
  match ranges with
  | [] -> false
  | (a, b) :: rs -> if id <= b && id >= a then true else valid_id id rs
;;

let part1 ranges ids =
  let rec helper acc ids =
    match ids with
    | [] -> acc
    | id :: ids -> helper (acc + Bool.to_int (valid_id id ranges)) ids
  in
  helper 0 ids


let input =
  open_in "input"
  |> In_channel.input_all
  |> stol

let ranges, ids =
  input
  |> parse_input
  |> Option.get
  |> fst
;;

let () = (part1 ranges ids) |> Int.to_string |> print_endline
