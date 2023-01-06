open Aoclib

module Types = struct
  type packet = Data of int | Packet of packet list
  [@@deriving show { with_path = false }]

  type input = (packet * packet) list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let data d = Data d
  let pack l = Packet l

  let packet =
    fix (fun p ->
        let p = enclosed '[' (sep_by (char ',') p) ']' in
        data <$> integer <|> (pack <$> p))
    <* end_of_line

  let input = sep_by end_of_line (both packet packet)
end

module Solving = struct
  open Base

  let rec compare_p p1 p2 =
    match (p1, p2) with
    | Data i, Data j -> compare i j
    | Data _, Packet pl -> compare_pl [ p1 ] pl
    | Packet pl, Data _ -> compare_pl pl [ p2 ]
    | Packet tl1, Packet tl2 -> compare_pl tl1 tl2

  and compare_pl p1 p2 =
    match (p1, p2) with
    | [], _ | _, [] -> Poly.compare p1 p2
    | h1 :: tl1, h2 :: tl2 -> (
        match compare_p h1 h2 with 0 -> compare_pl tl1 tl2 | d -> d)

  let flatten l = List.fold l ~init:[] ~f:(fun acc (a, b) -> b :: a :: acc)

  let part1 (input : input) : output =
    let well_ordered (a, b) = compare_p a b < 0 in
    List.foldi input ~init:0 ~f:(fun i acc p ->
        if well_ordered p then acc + i + 1 else acc)

  let part2 (input : input) : output =
    let div2 = Packet [ Packet [ Data 2 ] ] in
    let div6 = Packet [ Packet [ Data 6 ] ] in
    let sorted =
      div2 :: div6 :: flatten input |> List.sort ~compare:compare_p
    in
    let f div _ e = compare_p div e = 0 in
    let decoder2, _ = List.findi_exn sorted ~f:(f div2) in
    let decoder6, _ = List.findi_exn sorted ~f:(f div6) in
    (decoder2 + 1) * (decoder6 + 1)
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
