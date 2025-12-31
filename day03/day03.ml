open Aoc2025.Parser

let largest_pair l =
  List.fold_left
    (fun (a, b) d ->
       let max = Int.max a b in
       let curr = (a * 10) + b in
       if (max * 10) + d > curr then max, d else a, b)
    (0, 0)
    l
;;

let largest_subset l =
  let opt = Hashtbl.create 1 in
  let rec helper k l =
    if List.length l < k
    then 0
    else (
      match k with
      | 0 -> 0
      | _ ->
        (match l with
         | [] -> 0
         | x :: xs ->
           if Hashtbl.mem opt (k, l)
           then Hashtbl.find opt (k, l)
           else (
             let power = Int.of_float (10. ** float_of_int (k - 1)) in
             let take = (x * power) + helper (k - 1) xs in
             let skip = helper k xs in
             let res = Int.max take skip in
             Hashtbl.add opt (k, l) res;
             res)))
  in
  helper 12 l
;;

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

let part2 =
  let rec helper acc =
    alt
      (some digit <<| newline |*> fun l -> helper (acc + largest_subset l))
      (of_value acc)
  in
  helper 0
;;

let example = open_in "example" |> In_channel.input_all |> stol
let input = open_in "input" |> In_channel.input_all |> stol

let () =
  input
  |> part1
  |> Option.get
  |> fst
  |> Int.to_string
  |> String.cat "Part 1 answer: "
  |> print_endline
;;

let () =
  input
  |> part2
  |> Option.get
  |> fst
  |> Int.to_string
  |> String.cat "Part 2 answer: "
  |> print_endline
;;

let () =
  example
  |> part2
  |> Option.get
  |> fst
  |> Int.to_string
  |> String.cat "Part 2 Example: "
  |> print_endline
;;
