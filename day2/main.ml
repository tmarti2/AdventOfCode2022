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

  let convert = function
    | 'A' | 'X' -> R
    | 'B' | 'Y' -> P
    | 'C' | 'Z' -> S
    | _ -> failwith "not a shape"

  let is_shape = function
    | 'A' .. 'C' | 'X' .. 'Z' -> true
    | _ -> false

  let shape = satisfy is_shape >>| convert

  let input = sep_by end_of_line (both (shape <* char ' ') shape)
end

module Solving = struct
  open Base

  let value = function R -> 1 | P -> 2 | S -> 3

  let win   = function R -> P | P -> S | S -> R

  let lose  = function R -> S | P -> R | S -> P

  let score (p1,p2) =
    (value p2) +
    if Poly.(p1 = p2) then 3 else if Poly.(p2 = win p1) then 6 else 0

  let choose = function
    | (p1,R) -> (p1,lose p1)
    | (p1,P) -> (p1,p1)
    | (p1,S) -> (p1,win p1)

  let part1 (input : input) : output =
    List.sum ~f:score (module Int) input |> Option.some

  let part2 (input : input) : output =
    List.sum ~f:(Fn.compose score choose) (module Int) input |> Option.some
end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all