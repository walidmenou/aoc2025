(* parser combinators *)
type 'a parser = char list -> ('a * char list) option

let ( |*> ) p f =
  fun input ->
  match p input with
  | Some (x, rest) -> (f x) rest
  | None -> None
;;

let alt p q =
  fun input ->
  match p input with
  | None -> q input
  | result -> result
;;

let of_value v = fun input -> Some (v, input)

let rec many p = alt (some p) (of_value [])
and some p = p |*> fun x -> many p |*> fun xs -> of_value (x :: xs)

let map f p = p |*> fun r -> of_value (f r)
let maybe p = alt (map (fun r -> Some r) p) (of_value None)
let ( <<| ) p q = p |*> fun r -> q |*> fun _ -> of_value r
let ( |>> ) p q = p |*> fun _ -> q |*> fun r -> of_value r
let ( <+> ) p q = alt p q
let sep_by sep p = p |*> fun x -> many (sep |>> p) |*> fun xs -> of_value (x :: xs)

(* parsers *)
let satisfies p = function
  | c :: cs when p c -> Some (c, cs)
  | _ -> None
;;

let char c = satisfies (fun x -> c = x)
let newline = maybe (char '\n')

let digit_char =
  satisfies (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let digit =
  satisfies (function
    | '0' .. '9' -> true
    | _ -> false)
  |> map (fun c -> Char.code c - Char.code '0')
;;

let number = some digit |> map (List.fold_left (fun acc d -> (acc * 10) + d) 0)

let eof = function
  | [] -> Some ((), [])
  | _ -> None
;;

(* string conversion *)
let stol s = s |> String.to_seq |> List.of_seq
let ltos l = l |> List.to_seq |> String.of_seq

(* parser result handling *)

exception ParsingError
