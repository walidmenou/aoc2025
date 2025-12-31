open Aoc2025.Parser

type id = int * int

let parse_id =
  number |*> fun a -> char '-' |*> fun _ -> number |*> fun b -> of_value (a, b)
;;

let parse_input : id list parser =
  some (parse_id |*> fun id -> maybe (char ',') |*> fun _ -> of_value id)
;;

let rec range a b = if a > b then [] else a :: range (a + 1) b
let sum l = List.fold_left ( + ) 0 l

let index_of (i : string) (s : string) : int option =
  let rec helper start =
    try
      let sub = String.sub s start (String.length s - start) in
      if String.starts_with ~prefix:i sub then Some start else helper (start + 1)
    with
    | _ -> None
  in
  helper 1
;;

let is_invalid id =
  let id = Int.to_string id in
  if String.length id mod 2 <> 0
  then false
  else (
    let mid = String.length id / 2 in
    String.sub id 0 mid = String.sub id mid mid)
;;

let is_repeating id =
  let id = Int.to_string id in
  Option.get (index_of id (id ^ id)) < String.length id
;;

let solve =
  parse_input
  |*> fun ids ->
  of_value
    (List.fold_left
       (fun (r1, r2) (a, b) ->
          let part1_ids = List.filter is_invalid (range a b) in
          let part2_ids = List.filter is_repeating (range a b) in
          sum part1_ids + r1, sum part2_ids + r2)
       (0, 0)
       ids)
;;

let part1 = fun input -> solve input |> Option.get |> fst |> fst
let part2 = fun input -> solve input |> Option.get |> fst |> snd
let input = open_in "input" |> input_line |> stol

let () =
  input |> part1 |> Int.to_string |> print_endline;
  input |> part2 |> Int.to_string |> print_endline
;;
