open Aoclib

module Types = struct
  type input = string list
  [@@deriving show]

  type output = int option
  [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input = sep_by end_of_line word
end

module Solving = struct
  open Base

  let score c =
    let code = Char.to_int c - 64 in
    if code <= 26 then code + 26 else code - 32

  let find_common_part1 s =
    let len = String.length s in
    let comp1, comp2 =
      String.sub s ~pos:0 ~len:(len/2), String.sub s ~pos:(len/2) ~len:(len/2)
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
      find_common_part2 e1 e2 e3 |> score |> (+) acc |> aux_part2 tl
    | _ -> assert false

  let part1 (input : input) : output =
    List.sum ~f:(Fn.compose score find_common_part1) (module Int) input
    |> Option.some

  let part2 (input : input) : output =
    aux_part2 input 0 |> Option.some
end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all