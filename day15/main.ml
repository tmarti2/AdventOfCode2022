open Aoclib

module Types = struct

  type coord = {x : int ; y : int} [@@deriving show {with_path = false}]
  type input = (coord*coord) list [@@deriving show]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let make_coord x y = {x; y}
  let coord s =
    lift2 make_coord
      (string s *> neg_integer)
      (string ", y=" *> neg_integer)
  let line = both (coord "Sensor at x=") (coord ": closest beacon is at x=")
  let input = many (line <* end_of_line)

end

module Solving = struct
  open Base

  let dist a b =
    abs (a.x - b.x) + abs (a.y - b.y)

  let find_dims sensors =
    List.fold sensors ~init:(0,0) ~f:(fun (minx,maxx) (s,b) ->
        min (min minx s.x) b.x,
        max (max maxx s.x) b.x
      )

  let count_row sensors y (min,max) acc =
    let rec aux x acc =
      if x > max then acc
      else
        let p = {x;y} in
        let s = List.find sensors ~f:(fun (s,_,d) ->
          dist p s <= d
          ) in
        match s with
        | None -> aux (x+1) acc
        | Some (s,b,d) ->
          let next = s.x + (d - abs(s.y - y)) + 1 in
          let b = if b.y = y && b.x >= x then -1 else 0 in
          aux next (acc + (next - x) + b)

    in
    aux min acc

let find_row sensors y max =
  let rec aux x =
    if x > max then None
    else begin
      let nx =
        List.fold sensors ~init:x ~f:(fun x (s,_,d) ->
            if dist {x;y} s <= d then
              s.x + (d - abs(s.y - y)) + 1
            else
              x
          )
      in
      if nx = x then Some {x;y} else aux nx
    end
  in
  aux 0

let find_beacon sensors max =
  let rec aux y =
    if y > max then None
    else
      match find_row sensors y max with
      | None -> aux (y+1)
      | Some p -> Some p
  in
  aux 0

  let compute_dist sensors =
    List.map sensors ~f:(fun (s,b) -> (s,b, dist s b))

  let part1 (input : input) : output =
    let y = if List.length input > 14 then 2000000 else 10 in
    let (min,max) = find_dims input in
    let sensors = compute_dist input in
    count_row sensors y (min*2,max*2) 0

  let part2 (input : input) : output =
    let max = if List.length input > 14 then 4000000 else 20 in
    let sensors = compute_dist input in
    match find_beacon sensors max with
    | None -> assert false
    | Some p -> p.x * 4000000 + p.y

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
