open Aoclib

module Types = struct
  type input = int list list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input = sep_by end_of_line (many (integer <* end_of_line))
end

module Solving = struct
  open Base

  let part1 (input : input) : output =
    List.map ~f:(List.sum ~f:Fn.id (module Int)) input
    |> List.max_elt ~compare |> Option.value_exn

  let part2 (input : input) : output =
    List.map ~f:(List.sum ~f:Fn.id (module Int)) input
    |> List.sort ~compare:(fun a b -> compare b a)
    |> Fn.flip List.take 3
    |> List.sum (module Int) ~f:Fn.id
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
