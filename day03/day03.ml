open Aoc2025.Parser

(*
  "893" -> (0, 0); (0, 8); (8, 9); ()
*)

let largest_pair l =
  List.fold_left
    (fun (a, b) d ->
      let max = Int.max a b in
      let curr = a * 10 + b in
      if max * 10 + d > curr then (max, d) else (a, b))
    (0, 0)
    l
;;

let newline = maybe (char '\n')

let part1 =
  let rec helper acc =
    alt
      (some digit
      <<| newline
      |*> fun l ->
      let a, b = largest_pair l in
      helper (acc + (a * 10) + b))
      (of_value acc)
  in
  helper 0
;;

let input = open_in "input" |> In_channel.input_all |> stol
let () = input |> part1 |> Option.get |> fst |> Int.to_string |> print_endline
