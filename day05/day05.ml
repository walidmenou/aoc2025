open Aoc2025.Parser;;

module IntSet = Set.Make(Int);;

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

let compare (lo1, hi1) (lo2, hi2) =
  match Int.compare lo1 lo2 with
  | 0 -> Int.compare hi1 hi2
  | o -> o
;;

let merged ranges =
  ranges
  |> List.sort compare
  |> List.fold_left
            (fun acc (lo2, hi2) ->
                match acc with
                | (lo1, hi1) :: rs when lo2 <= hi1 -> (lo1, max hi1 hi2) :: rs
                | _ -> (lo2, hi2) :: acc) []
;;

let range_size (lo, hi) = hi - lo + 1
let sum = List.fold_left ( + ) 0
let part2 ranges =
  ranges
  |> merged
  |> List.map range_size
  |> sum

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
let () = (part2 ranges) |> Int.to_string |> print_endline
