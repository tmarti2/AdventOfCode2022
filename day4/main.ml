open Aoclib

module Types = struct
  type task = { min : int; max : int } [@@deriving show]
  type input = (task * task) list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let create_task (min, max) = { min; max }
  let task = both (integer <* char '-') integer >>| create_task
  let pair = both (task <* char ',') task
  let input = many (pair <* end_of_line)
end

module Solving = struct
  open Base

  let contains a b = a.min <= b.min && a.max >= b.max
  let included (a, b) = contains a b || contains b a
  let overlap (a, b) = if a.min < b.min then b.min <= a.max else a.min <= b.max
  let part1 (input : input) : output = List.count ~f:included input
  let part2 (input : input) : output = List.count ~f:overlap input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
