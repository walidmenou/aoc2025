open Aoc2025.Parser

module Coord : sig
  type t = int * int

  val compare : t -> t -> int
end = struct
  type t = int * int

  let compare =
    fun (px, py) (qx, qy) ->
    match Int.compare px qx with
    | 0 -> Int.compare py qy
    | o -> o
  ;;
end

module CoordSet = Set.Make (Coord)

(* global variables for coordinates *)
let gx = ref 0
let gy = ref 0

(* parsers with effects *)
let increment r =
  fun input ->
  r := !r + 1;
  Some ((), input)
;;

let reset r =
  fun input ->
  r := 0;
  Some ((), input)
;;

let empty = char '.' |*> fun _ -> of_value []
let paper = char '@' |*> fun _ -> of_value [ !gx, !gy ]
let newline = char '\n' |>> reset gx |>> increment gy
let location = empty <+> paper <<| increment gx
let line = some location |> map List.concat

let input =
  sep_by newline line <<| maybe newline <<| eof |> map List.concat |> map CoordSet.of_list
;;

let neighbors_of (x, y) =
  [ x + 1, y + 0
  ; x + 0, y + 1
  ; x - 1, y + 0
  ; x + 0, y - 1
  ; x + 1, y + 1
  ; x - 1, y - 1
  ; x + 1, y - 1
  ; x - 1, y + 1
  ]
;;

let should_remove grid (x, y) =
  neighbors_of (x, y)
  |> List.filter (fun neighbor -> CoordSet.mem neighbor grid)
  |> List.length
  |> fun amount -> amount < 4
;;

let parsed_input =
  open_in "input"
  |> In_channel.input_all
  |> stol
  |> input
  |> function
  | Some (x, _) -> x
  | _ -> raise ParsingError
;;

let part1 =
  fun input -> input |> CoordSet.filter (should_remove input) |> CoordSet.cardinal
;;


let part2 = fun input ->
  let rec helper acc grid =
    let to_remove = grid |> CoordSet.filter (should_remove grid) in
    match CoordSet.cardinal to_remove with
    | 0 -> acc
    | x ->
        let new_grid = CoordSet.diff grid to_remove in
        helper (acc + x) new_grid
  in
  helper 0 input


let () = parsed_input |> part1 |> Int.to_string |> print_endline
let () = parsed_input |> part2 |> Int.to_string |> print_endline
