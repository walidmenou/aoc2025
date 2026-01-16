open Aoc2025.Parser

type operation =
  | Add
  | Multiply

let newline = char '\n'
let space = char ' '
let spaces = many space
let operator = alt (char '*' |>> of_value Multiply) (char '+' |>> of_value Add)
let operand = number <<| spaces |*> fun x -> of_value x
let line = maybe spaces |>> many operand <<| newline

let parse_input =
  many line
  |*> fun grid ->
  sep_by (many space) operator |*> fun operations -> of_value (grid, operations)
;;

let transpose (m : 'a list list) =
  let rec helper (m : 'a list list) (acc : 'a list list) : 'a list list =
    match m with
    | [] | [] :: _ -> List.rev acc
    | _ :: _ -> helper (List.map List.tl m) (List.map List.hd m :: acc)
  in
  helper m []
;;

(* *)

let parsed_input = open_in "input" |> In_channel.input_all |> stol |> parse_input
let grid, ops = parsed_input |> Option.get |> fst
let grid = transpose grid

let part1 grid ops =
  let rec helper grid ops acc =
    match grid, ops with
    | [], _ | _, [] -> acc
    | col :: grid, Add :: ops -> helper grid ops (List.fold_left ( + ) 0 col + acc)
    | col :: grid, Multiply :: ops -> helper grid ops (List.fold_left ( * ) 1 col + acc)
  in
  helper grid ops 0
;;

let () = part1 grid ops |> Int.to_string |> print_endline
