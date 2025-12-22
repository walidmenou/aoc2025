open Aoc2025.Parser

type id = int * int

let parse_id =
  number |*> fun a -> char '-' |*> fun _ -> number |*> fun b -> of_value (a, b)
;;

let parse_input : id list parser =
  some (parse_id |*> fun id -> maybe (char ',') |*> fun _ -> of_value id)
;;

(* helpers to determine if string is valid or not *)
let index_of (i : string) (s : string) : int option =
  let rec helper start =
    try
      let sub = String.sub s start (String.length s - start) in
      if String.starts_with ~prefix:i sub then Some start else helper (start + 1)
    with
    | _ -> None
  in
  helper 0
;;

let is_invalid id =
  let id = Int.to_string id in
  Option.get (index_of id (id ^ id)) <> String.length id
;;

let rec range a b = if a > b then [] else a :: range (a + 1) b
let sum l = List.fold_left ( + ) 0 l

let part1 =
  parse_input
  |*> fun ids ->
  of_value
    (List.fold_left (fun r (a, b) -> sum (List.filter is_invalid (range a b)) + r) 0 ids)
;;

let input = In_channel.stdin |> In_channel.input_all |> stol
let () = input |> part1 |> Option.get |> fst |> Int.to_string |> print_endline
