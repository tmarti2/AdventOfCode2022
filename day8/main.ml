open Aoclib

module Types = struct

  let pp_array pp_elem fmt s =
    let pp_sep ppf () = Format.fprintf ppf "|" in
    Array.to_seq s |> Format.pp_print_seq ~pp_sep pp_elem fmt
  type input = int array array
  let pp_input fmt s =
    Array.iter (fun e ->
      pp_array Format.pp_print_int fmt e;
      Format.pp_print_newline fmt ()) s

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing
  open Base

  let to_int l =
    List.map l ~f:(fun c -> Char.(to_int c - to_int '0'))

  let row = take_while number >>| (Array.of_list << to_int << String.to_list)
  let input = many (row <* end_of_line) >>| Array.of_list
end

module Solving = struct
  open Base

  let dims map = Array.length map.(0), Array.length map

  let is_inside x y dx dy =
    x >= 0 && x < dx && y >= 0 && y < dy

  let visible_dir map x y (dx,dy) =
    let dimx, dimy = dims map in
    let v = map.(y).(x) in
    let rec aux x y =
      let x', y' = x + dx, y + dy in
      if not @@ is_inside x' y' dimx dimy then true
      else if map.(y').(x') >= v then false else aux x' y'
    in
    aux x y

  let is_visible map x y =
    List.exists ~f:(visible_dir map x y) [(1,0);(0,1);(-1,0);(0,-1)]

  let count_visibility map =
    Array.foldi ~init:0 ~f:(fun y acc row ->
        acc + Array.counti ~f:(fun x _ ->
            is_visible map x y
          ) row
      ) map

  let view_distance map x y (dx,dy) =
    let dimx, dimy = dims map in
    let v = map.(y).(x) in
    let rec aux x y dist =
      let x', y' = x + dx, y + dy in
      if not @@ is_inside x' y' dimx dimy then dist
      else if map.(y').(x') >= v then dist + 1 else aux x' y' (dist+1)
    in
    aux x y 0

  let scenic_score map x y =
    List.map ~f:(view_distance map x y) [(1,0);(0,1);(-1,0);(0,-1)]
    |> List.reduce_exn ~f:( * )

  let best_scenic_score map =
    Array.foldi ~init:0 ~f:(fun y init row ->
        Array.foldi ~init ~f:(fun x acc _ ->
            max acc (scenic_score map x y)
          ) row
      ) map

  let part1 (input : input) : output =
    count_visibility input

  let part2 (input : input) : output =
    best_scenic_score input

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
