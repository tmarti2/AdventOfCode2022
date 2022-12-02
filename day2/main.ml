open Aoclib

module Types = struct
  type shape = R | S | P
  [@@deriving show]
  type input = (shape*shape) list
  [@@deriving show]

  type output = int option
  [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let shape_of_string = function
    | "A" | "X" -> R
    | "B" | "Y" -> P
    | "C" | "Z" -> S
    | _ -> failwith "wrong shape"

  let shape = letter >>| shape_of_string

  let input =
    sep_by end_of_line (both shape (skip_many space *> shape))
end

module Solving = struct
  open Base

  let value = function
    | R -> 1
    | P -> 2
    | S -> 3

  let win = function
    | R -> P
    | P -> S
    | S -> R

  let lose = function
    | R -> S
    | P -> R
    | S -> P

  let score = function
    | (p1,p2) ->
      (value p2) +
      if Poly.(p1 = p2) then 3 else if Poly.(p2 = win p1) then 6 else 0

  let choose = function
    | (p1,R) -> (p1,lose p1)
    | (p1,P) -> (p1,p1)
    | (p1,S) -> (p1,win p1)

  let part1 (input : input) : output =
    List.sum ~f:score (module Int) input
    |> Option.some

  let part2 (input : input) : output =
    List.sum ~f:(fun i -> score (choose i)) (module Int) input
    |> Option.some
end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all