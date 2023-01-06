open Aoclib

module Types = struct
  type input = string [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input = word <* end_of_line
end

module Solving = struct
  open Base

  let solve str size =
    let rec aux n =
      let curr_size =
        String.sub str ~pos:(n - size) ~len:size
        |> String.to_list
        |> Set.of_list (module Char)
        |> Set.length
      in
      if curr_size = size then n else aux (n + 1)
    in
    aux size

  let part1 (input : input) : output = solve input 4
  let part2 (input : input) : output = solve input 14
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
