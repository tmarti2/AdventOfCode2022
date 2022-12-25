open Aoclib

module Types = struct
  type symbol = Zero | One | Two | Minus | Equal
  [@@deriving show {with_path = false}]

  type input = symbol list list [@@deriving show]

  type output = string [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom

  let zero = char '0' *> return Zero
  let one = char '1' *> return One
  let two = char '2' *> return Two
  let minus = char '-' *> return Minus
  let equal = char '=' *> return Equal
  let snafu = many1 (zero <|> one <|> two <|> minus <|> equal)

  let input = many (snafu  <* end_of_line)
end

module Solving = struct
  open Base

  let symb_to_string = function
    | Zero -> "0"
    | One -> "1"
    | Two -> "2"
    | Minus -> "-"
    | Equal -> "="

  let snafu_to_string symbs =
    List.map symbs ~f:symb_to_string |> String.concat

  let to_dec = function
    | Zero -> 0
    | One -> 1
    | Two -> 2
    | Minus -> -1
    | Equal -> -2

  let snafu_to_dec symbs =
    List.fold symbs ~init:0 ~f:(fun acc symb ->
        5 * acc + (to_dec symb)
      )

  let rest_remainder = function
    | 0 -> Zero, 0
    | 1 -> One, 0
    | 2 -> Two, 0
    | 3 -> Minus, 1
    | 4 -> Equal, 1
    | _ -> assert false

  let dec_to_snafu n =
    let rec to_snafu acc = function
      | 0 -> acc
      | n ->
        let symb, r = rest_remainder (n % 5) in
        to_snafu (symb :: acc) (n / 5 + r)
    in
    if n = 0 then [Zero] else to_snafu [] n

  let part1 (input : input) : output =
    List.sum (module Int) input ~f:snafu_to_dec
    |> dec_to_snafu |> snafu_to_string

  let part2 (_input : input) : output = "0"
end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
