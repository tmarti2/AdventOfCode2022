open Aoclib

module Types = struct
  type input = int list list
  [@@deriving show]

  type output = int option
  [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input =
    sep_by end_of_line (sep_by end_of_line integer)
end

module Solving = struct
  open Base

  let part1 (input : input) : output =
    List.map ~f:(List.sum ~f:(Fn.id) (module Int)) input
    |> List.max_elt ~compare

  let part2 (input : input) : output =
    let all =
      List.map ~f:(List.sum ~f:(Fn.id) (module Int)) input
      |> List.sort ~compare:(fun a b -> compare b a)
    in
    List.sum (module Int) ~f:(Fn.id) (List.take all 3)
    |> Option.some
end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all