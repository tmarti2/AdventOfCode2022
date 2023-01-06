open Aoclib

module Types = struct
  type input = (int * int) list list [@@deriving show { with_path = false }]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let coord = both integer (char ',' *> integer)
  let path = sep_by (string " -> ") coord <* end_of_line
  let input = many path
end

module Solving = struct
  open Base

  type v = Rock | Air | Source | Sand

  let dims paths =
    List.fold paths ~init:(0, 0) ~f:(fun init p ->
        List.fold p ~init ~f:(fun (maxx, maxy) (x, y) ->
            (max x maxx, max y maxy)))

  let in_range (maxx, maxy) (x, y) = x >= 0 && x < maxx && y >= 0 && y < maxy
  let is_air map (x, y) = Poly.(map.(y).(x) = Air)

  type status = Stop of int | Continue of int

  let fall_until dims map =
    let fall_down acc =
      let rec aux (x, y) =
        let next_pos = [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ] in
        let f = List.filter next_pos ~f:(in_range dims) in
        match f with
        | [] -> Stop acc
        | l -> (
            match (List.find l ~f:(is_air map), map.(y).(x)) with
            | None, Source ->
                map.(y).(x) <- Sand;
                Stop (acc + 1)
            | None, _ ->
                map.(y).(x) <- Sand;
                Continue (acc + 1)
            | Some p, _ -> aux p)
      in
      aux (500, 0)
    in
    let rec aux acc =
      match fall_down acc with Stop acc -> acc | Continue acc -> aux acc
    in
    aux 0

  let init_map ?bot dimy dimx rocks =
    let map = Array.make_matrix ~dimx ~dimy Air in
    let fill_rock map (x, y) (x', y') =
      let srcx, dstx = (min x x', max x x') in
      let srcy, dsty = (min y y', max y y') in
      for i = srcx to dstx do
        map.(y).(i) <- Rock
      done;
      for j = srcy to dsty do
        map.(j).(x) <- Rock
      done
    in
    let rec iter_path = function
      | src :: dst :: tl ->
          fill_rock map src dst;
          iter_path (dst :: tl)
      | _ -> ()
    in
    map.(0).(500) <- Source;
    List.iter rocks ~f:iter_path;
    match bot with
    | None -> map
    | Some v ->
        Array.iteri map.(v) ~f:(fun x _ -> map.(v).(x) <- Rock);
        map

  let part1 (input : input) : output =
    let maxx, maxy = dims input in
    let dimx, dimy = (maxx + 1, maxy + 1) in
    init_map dimx dimy input |> fall_until (dimx, dimy)

  let part2 (input : input) : output =
    let _, maxy = dims input in
    let dimx, dimy = (1000, maxy + 3) in
    init_map ~bot:(dimy - 1) dimx dimy input |> fall_until (dimx, dimy)
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
