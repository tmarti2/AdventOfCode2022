open Aoclib

module Types = struct
  type instr = Nop | Addx of int
  [@@deriving show {with_path = false}]

  type input = instr list
  [@@deriving show {with_path = false}]

  type output = int
  [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let int_neg = char '-' *> integer >>| (-) 0
  let int = integer <|> int_neg

  let nop = string "noop" *> return Nop
  let add = string "addx " *> int >>| fun i -> Addx i

  let input = many (nop <|> add <* end_of_line)
end

module Solving = struct
  open Base

  let do_instr f (cycles,x,res) i =
    let res = f (cycles , x, res) i in
    match i with
    | Nop -> cycles+1, x, res
    | Addx v -> cycles+2, x+v, res

  let execute instrs f =
    List.fold instrs ~init:(0,1,0) ~f:(do_instr f)

  let aux_part1 (cycles,x,res) instr =
    match instr with
    | Nop ->
      if (cycles + 1) % 40 = 20
      then res + x * (cycles + 1)
      else res
    | Addx _ ->
      if (cycles + 1) % 40 = 20
      then res + (x * (cycles + 1))
      else if (cycles + 2) % 40 = 20
        then res + (x * (cycles + 2))
      else res

  let part1 (input : input) : output =
    let (_,_,res) = execute input aux_part1 in res

  let part2 (_input : input) : output =
    Stdlib.Format.printf ":ResidentSleeper:@.%!"; 0

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
