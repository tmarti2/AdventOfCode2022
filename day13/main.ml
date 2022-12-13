open Aoclib

module Types = struct
  type packet = Data of int | Packet of packet list
  [@@deriving show {with_path = false}]
  type input = (packet list * packet list) list
  [@@deriving show {with_path = false}]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let int = integer >>| fun i -> Data i
  let pack l = Packet l
  let packets =
    fix (fun packets ->
      let packet = int <|> (pack <$> packets) in
      char '[' *> sep_by (char ',') packet <* char ']'
    )

  let packet = packets <* end_of_line
  let input = sep_by1 end_of_line (both packet packet)

end

module Solving = struct
  open Base

  let rec compare p1 p2 =
    match p1, p2 with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | Data i :: tl, Packet _ :: _ -> compare (Packet [Data i] :: tl) p2
    | Packet _ :: _, Data j :: tl -> compare p1 (Packet [Data j] :: tl)
    | Data i :: tl1 , Data j :: tl2 ->
      begin match Int.compare i j with
        | 0 -> compare tl1 tl2
        | d -> d
      end
    | Packet p1 :: tl1, Packet p2 :: tl2 ->
      begin match compare p1 p2 with
      | 0 -> compare tl1 tl2
      | d -> d
      end

  let flatten l =
    List.fold l ~init:[] ~f:(fun acc (a,b) -> b :: a :: acc)
    |> List.rev

  let part1 (input : input) : output =
    List.foldi input ~init:0 ~f:(fun i acc (p1,p2) ->
        if compare p1 p2 <= 0 then acc + i + 1 else acc
      )

  let part2 (input : input) : output =
    let d2, d6 = [Packet [Data 2]],[Packet [Data 6]] in
    let sorted = (d2,d6)::input |> flatten |> List.sort ~compare:compare in
    let f divider _ e = compare divider e = 0 in
    let decoder2, _ = List.findi_exn sorted ~f:(f d2) in
    let decoder6, _ = List.findi_exn sorted ~f:(f d6) in
    (decoder2 + 1) * (decoder6 + 1)

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
