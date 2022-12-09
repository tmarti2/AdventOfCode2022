open Aoclib

module Types = struct
  type dir = Up | Down| Left | Right
  [@@deriving show {with_path = false}]
  type action = {dir:dir; steps:int}
  [@@deriving show {with_path = false}]

  type input = action list [@@deriving show {with_path = false}]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let make_action dir steps = {dir;steps}
  let dir =
    char 'R' *> return Right
    <|> char 'U' *> return Up
    <|> char 'L' *> return Left
    <|> char 'D' *> return Down
  let action = lift2 make_action dir (space *> integer)

  let input = many (action <* end_of_line)
end

module Solving = struct
  open Base

  module IntPair = struct
    module T = struct
      type t = {x:int;y:int} [@@deriving show {with_path = false}, compare, sexp_of]
    end

    include T
    include Comparable.Make(T)
  end
  open IntPair.T

  let move_head p = function
    | Up -> {p with y = p.y+1}
    | Down -> {p with y = p.y-1}
    | Right -> {p with x = p.x+1}
    | Left -> {p with x = p.x-1}

  let move_tail prev next =
    let diff p1 p2 = p1.x - p2.x, p1.y-p2.y in
    let dx,dy = diff prev next in
    let clamp d = Int.clamp_exn ~min:(-1) ~max:1 d in
    let next =
      let adjust () = { x = next.x + clamp dx; y = next.y + clamp dy } in
      if abs dx >= 2 || abs dy >= 2 then adjust () else next
    in
    next

  let move_all dir = function
    | [] -> assert false
    | head :: tail ->
      let head = move_head head dir in
      let f prev next =
        let next = move_tail prev next in
        next, next
      in
      head :: List.folding_map tail ~init:head ~f

  let do_action init a =
    let actions = List.init a.steps ~f:(fun _ -> a.dir) in
    List.fold actions ~init ~f:(fun (knots,visited) dir ->
        let new_knots = move_all dir knots in
        let new_visited = List.last_exn new_knots |> Set.add visited in
        (new_knots, new_visited)
      )

  let compute actions nb_knots =
    let knots = List.init nb_knots ~f:(fun _ -> {x=0;y=0}) in
    let visited = Set.empty (module IntPair) in
    let _,visited = List.fold actions ~init:(knots,visited) ~f:do_action in
    Set.length visited

  let part1 (input : input) : output = compute input 2

  let part2 (input : input) : output = compute input 10

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
