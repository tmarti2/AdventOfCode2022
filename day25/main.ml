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

  let snafu_to_string symbs =
    let symb_to_string = function
      | Zero -> "0"
      | One -> "1"
      | Two -> "2"
      | Minus -> "-"
      | Equal -> "="
    in
    List.map symbs ~f:symb_to_string |> String.concat

  let to_dec = function
    | Zero -> 0
    | One -> 1
    | Two -> 2
    | Minus -> -1
    | Equal -> -2

  let snafu_to_dec symbs =
    List.rev symbs
    |> List.foldi ~init:0 ~f:(fun i acc symb ->
        acc + (5 ** i) * (to_dec symb)
      )

  let dec_to_snafu n =
    let rec from_dec acc = function
      | -2 -> Equal :: acc
      | -1 -> Minus :: acc
      | 0 -> Zero :: acc
      | 1 -> One :: acc
      | 2 -> Two :: acc
      | n -> to_snafu acc n
    and to_snafu acc n =
      let q, r = n / 5, n % 5 in
      let r, c = if r <= 2 then r, 0 else (r - 5), 1 in
      from_dec (from_dec acc r) (c + q)
    in
    from_dec [] n

  let part1 (input : input) : output =
  List.sum (module Int) input ~f:snafu_to_dec
  |> dec_to_snafu |> snafu_to_string

  let part2 (_input : input) : output = "0"
end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
