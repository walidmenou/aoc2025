open Aoc2025
open Parser

let rotation =
  alt (char 'L') (char 'R')
  |*> fun d ->
  number |*> fun n -> maybe (char '\n') |*> fun _ -> of_value (if d = 'R' then n else -n)
;;

let part1 =
  let rec helper pos count =
  alt
    (rotation
      |*> fun n ->
          let pos = (pos + n) mod 100 in
          if pos = 0 then helper pos (count + 1) else helper pos count)
    (of_value count)
  in
  helper 50 0

let part2 =
  let rec helper pos count =
    alt
      (rotation
      |*> fun n ->
          let pos = pos + n in
          let count = count + (Int.abs(pos) / 100) + Bool.to_int(pos <= 0 && pos - n <> 0) in
          let pos = (pos mod 100 + 100) mod 100 in
          helper pos count)
      (of_value count)
  in
  helper 50 0
;;

let input = In_channel.stdin |> In_channel.input_all |> stol

let _ =
  input |> part1 |> Option.get |> fst |> Int.to_string |> print_endline;
  input |> part2 |> Option.get |> fst |> Int.to_string |> print_endline
;;
