open Aoclib

module Types = struct
  type input = char list list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let row = word <* end_of_line >>| Base.String.to_list
  let input = many1 row
end

module Solving = struct
  open Base

  module Coord = struct
    type t = { x : int; y : int } [@@deriving show]

    let compare = Poly.compare
    let hash = Hashtbl.hash
    let equal = Poly.( = )
  end
  [@@warning "-69"]

  module G = Graph.Persistent.Digraph.ConcreteBidirectional (Coord)

  module W = struct
    type edge = G.E.t
    type t = int

    let weight _ = 1
    let compare = compare
    let add = ( + )
    let zero = 0
  end

  module GP = Graph.Path.Dijkstra (G) (W)
  module GB = Graph.Path.BellmanFord (G) (W)
  module GO = Graph.Oper.P (G)

  let create_map input =
    Array.of_list_map input ~f:(fun row -> Array.of_list row)

  let find_se map =
    let s, e =
      Array.foldi map ~init:(None, None) ~f:(fun y init row ->
          Array.foldi row ~init ~f:(fun x (s, e) c ->
              match c with
              | 'S' ->
                  map.(y).(x) <- 'a';
                  (Some Coord.{ x; y }, e)
              | 'E' ->
                  map.(y).(x) <- 'z';
                  (s, Some Coord.{ x; y })
              | _ -> (s, e)))
    in
    (Option.value_exn s, Option.value_exn e)

  let in_range (w, h) (x, y) = x >= 0 && x < w && y >= 0 && y < h

  let higher c c' =
    let c, c' = (Char.to_int c, Char.to_int c') in
    c' <= c + 1

  let find_neighbors map size (x, y) =
    let deltas = [ (-1, 0); (0, -1); (1, 0); (0, 1) ] in
    let coords = List.map ~f:(fun (dx, dy) -> (x + dx, y + dy)) deltas in
    List.filter ~f:(in_range size) coords
    |> List.filter ~f:(fun (x', y') -> higher map.(y).(x) map.(y').(x'))

  let compute_graph map =
    let size = (Array.length map.(0), Array.length map) in
    Array.foldi map ~init:G.empty ~f:(fun y g row ->
        Array.foldi row ~init:g ~f:(fun x g _ ->
            List.fold ~init:g
              ~f:(fun g (x', y') -> G.add_edge g { x; y } { x = x'; y = y' })
              (find_neighbors map size (x, y))))

  let part1 (input : input) : output =
    let map = create_map input in
    let s, e = find_se map in
    let g = compute_graph map in
    snd @@ GP.shortest_path g s e

  let part2 (input : input) : output =
    let map = create_map input in
    let _, e = find_se map in
    let g = compute_graph map |> GO.mirror in
    let h = GB.all_shortest_paths g e in
    GB.H.fold
      (fun Coord.{ x; y } d acc ->
        if Poly.(map.(y).(x) = 'a') then d :: acc else acc)
      h []
    |> List.min_elt ~compare |> Option.value_exn
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
