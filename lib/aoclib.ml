module type Types = sig
  type input
  type output
  val pp_output : Format.formatter -> output -> unit
end

module type Parsing = sig
  type input
  val input : input Angstrom.t
end

module type Solving = sig
  type input
  type output
  val part1 : input -> output
  val part2 : input -> output
end

module Parsing = struct
  open Angstrom

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let word =
    take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
end

module MakeDay
    (T : Types)
    (P : Parsing with type input := T.input)
    (S : Solving with type input := T.input and type output := T.output) =
struct

  let go file =
    Format.printf "%s@.%!" file;
    let do_parse str =
      let open Angstrom in
      match parse_string ~consume:All P.input str with
      | Ok v -> v
      | Error msg -> failwith msg
    in
    let input = Stdio.In_channel.read_all file |> do_parse in
    let t1 = Unix.gettimeofday () in
    let o1 = S.part1 input in
    let t2 = Unix.gettimeofday () in
    let o2 = S.part2 input in
    let t3 = Unix.gettimeofday () in
    Format.printf "Part 1: %a in %fs@.%!" T.pp_output o1 (t2-.t1);
    Format.printf "Part 2: %a in %fs@.%!" T.pp_output o2 (t3-.t2)

  let run_all = go "example.txt"; go "input.txt"
end

(* Compose *)
let (<<) f g x = f (g x)