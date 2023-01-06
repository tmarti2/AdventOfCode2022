open Aoclib

module Types = struct
  type input = string list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input = many (word <* end_of_line)
end

module Solving = struct
  open Base

  let priority c =
    match c with
    | 'a' .. 'z' -> Char.to_int c - 96
    | 'A' .. 'Z' -> Char.to_int c - 38
    | _ -> assert false

  let find_common_part1 s =
    let mid = String.length s / 2 in
    let comp1, comp2 =
      (String.sub s ~pos:0 ~len:mid, String.sub s ~pos:mid ~len:mid)
    in
    String.find comp1 ~f:(fun c1 -> String.exists comp2 ~f:(Char.equal c1))
    |> Option.value_exn

  let find_common_part2 s1 s2 s3 =
    String.filter s1 ~f:(fun c1 -> String.exists s2 ~f:(Char.equal c1))
    |> String.find ~f:(fun c1 -> String.exists s3 ~f:(Char.equal c1))
    |> Option.value_exn

  let rec aux_part2 todo acc =
    match todo with
    | [] -> acc
    | e1 :: e2 :: e3 :: tl ->
        find_common_part2 e1 e2 e3 |> priority |> ( + ) acc |> aux_part2 tl
    | _ -> assert false

  let part1 (input : input) : output =
    List.sum ~f:(priority << find_common_part1) (module Int) input

  let part2 (input : input) : output = aux_part2 input 0
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
