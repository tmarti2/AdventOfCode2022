open Aoclib

module Types = struct
  type coord = { x : int; y : int } [@@deriving show { with_path = false }]
  type input = (coord * coord) list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let make_coord x y = { x; y }

  let coord s =
    lift2 make_coord (string s *> neg_integer) (string ", y=" *> neg_integer)

  let line = both (coord "Sensor at x=") (coord ": closest beacon is at x=")
  let input = many (line <* end_of_line)
end

module Solving = struct
  open Base

  let dist a b = abs (a.x - b.x) + abs (a.y - b.y)

  type interval = { l : int; h : int; b : coord }

  let intervals sensors y =
    List.filter_map sensors ~f:(fun (s, b, d) ->
        let d = d - abs (s.y - y) in
        if d < 0 then None else Some { l = s.x - d; h = s.x + d; b })

  let count_row y sensors =
    let count_b b x = if b.y = y && b.x >= x then -1 else 0 in
    let intervals = intervals sensors y |> List.sort ~compare:Poly.compare in
    let diff l h = h - l + 1 in
    match intervals with
    | [] -> 0
    | { l; h; _ } :: tl ->
        snd
        @@ List.fold tl
             ~init:(h + 1, diff l h - 1)
             ~f:(fun (x, acc) { l; h; b } ->
               let b = count_b b x in
               if x > h then (x, acc)
               else if x < l then (h + 1, acc + diff l h + b)
               else (h + 1, acc + (h - x) + b + 1))

  let find_row sensors y max =
    let rec aux x =
      if x > max then None
      else
        let nx =
          List.fold sensors ~init:x ~f:(fun x (s, _, d) ->
              if dist { x; y } s <= d then s.x + (d - abs (s.y - y)) + 1 else x)
        in
        if nx = x then Some { x; y } else aux nx
    in
    aux 0

  let find_beacon max sensors =
    let rec aux y =
      if y > max then None
      else
        match find_row sensors y max with
        | None -> aux (y + 1)
        | Some p -> Some p
    in
    aux 0

  let compute_dist sensors =
    List.map sensors ~f:(fun (s, b) -> (s, b, dist s b))

  let part1 (input : input) : output =
    let y = if List.length input = 14 then 10 else 2000000 in
    compute_dist input |> count_row y

  let part2 (input : input) : output =
    let max = if List.length input = 14 then 20 else 4000000 in
    match compute_dist input |> find_beacon max with
    | None -> assert false
    | Some p -> (p.x * 4000000) + p.y
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
