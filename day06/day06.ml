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

let input = open_in "input" |> In_channel.input_all |> stol
let parsed_input = parse_input input
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

(* Part 2 *)

let raw_line = many (alt space digit_char) <<| newline

let p2_parse =
  many raw_line
  |*> fun grid -> sep_by spaces operator |*> fun operators -> of_value (grid, operators)
;;

let rec concat = function
  | [] -> []
  | x :: xs -> (x @ [ ';' ]) @ concat xs
;;

let p2_input =
  let grid, ops = p2_parse input |> Option.get |> fst in
  let grid =
    grid
    |> List.map List.rev
    |> transpose
    |> List.map (List.filter (fun c -> c <> ' '))
    |> List.map (function
      | [] -> [ '\n' ]
      | xs -> xs)
    |> concat
  in
  let ops = List.rev ops in
  grid, ops
;;

let semicolon = char ';'

let p2_numbers =
  sep_by newline (maybe semicolon |>> sep_by semicolon number <<| maybe semicolon)
;;

let part2 =
  let grid, ops = p2_input in
  let grid = grid |> p2_numbers |> Option.get |> fst in
  part1 grid ops
;;

let () = part1 grid ops |> Int.to_string |> print_endline
let () = part2 |> Int.to_string |> print_endline
