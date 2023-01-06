open Aoclib

module Types = struct
  type node = { flow : int; dest : string list }
  [@@deriving show { with_path = false }]

  type input = (string * node) list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let valve = string "Valve " *> word
  let flow = string " has flow rate=" *> integer
  let dest1 = string "; tunnel leads to valve " *> word >>| fun w -> [ w ]
  let destn = string "; tunnels lead to valves " *> sep_by (string ", ") word

  let row =
    lift3
      (fun name flow dest -> (name, { flow; dest }))
      valve flow (dest1 <|> destn)

  let input = many1 (row <* end_of_line)
end

module Solving = struct
  open Base

  module Node = struct
    type t = string

    let compare = Poly.compare
    let hash = Hashtbl.hash
    let equal = Poly.( = )
  end

  module G = Graph.Persistent.Digraph.ConcreteBidirectional (Node)

  module W = struct
    type edge = G.E.t
    type t = int

    let weight _ = 1
    let compare = compare
    let add = ( + )
    let sub = ( - )
    let zero = 0
  end

  module GJ = Graph.Path.Johnson (G) (W)

  let all_distances input =
    List.fold input ~init:G.empty ~f:(fun init (name, { dest; _ }) ->
        List.fold ~init dest ~f:(fun g n -> G.add_edge g name n))
    |> GJ.all_pairs_shortest_paths

  let interesting_nodes input =
    List.filter_map input ~f:(fun (name, { flow; _ }) ->
        if flow > 0 then Some (name, flow) else None)

  let search input =
    let distances = all_distances input in
    let rec aux time_left valves_left score pos =
      if time_left < 0 then score
      else
        List.map valves_left ~f:(fun (next, flow) ->
            let time = time_left - GJ.HVV.find distances (pos, next) - 1 in
            let score = score + max 0 (time * flow) in
            let left =
              List.filter valves_left ~f:(fun (s, _) ->
                  not (String.equal s next))
            in
            aux time left score next)
        |> List.max_elt ~compare
        |> Option.value ~default:score
    in
    aux 30 (interesting_nodes input) 0 "AA"

  let part1 (input : input) : output = search input

  let part2 (_input : input) : output =
    Stdlib.Format.printf "P2 TODO@.%!";
    0
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
