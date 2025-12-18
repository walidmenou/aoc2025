open Aoc2025;;
open Parser;;

let rotation =
  alt (char 'L') (char 'R')
  |*> fun d -> number
  |*> fun n -> maybe (char '\n')
  |*> fun _ -> of_value (if d = 'R' then n else -n)
;;

let zeros =
  let rec helper pos count =
    alt
    (rotation
    |*> fun n ->
        let new_pos = pos + n in
        let count = count + Int.abs((new_pos / 100) - (pos / 100)) in
        helper new_pos count)
    (of_value count)
  in
  helper 50 0
;;


let _ =
  In_channel.stdin
  |> In_channel.input_all
  |> stol
  |> zeros
  |> Option.get
  |> fst
  |> Int.to_string
  |> print_endline
