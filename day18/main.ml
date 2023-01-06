open Aoclib

module Types = struct
  type input = (int * int * int) list [@@deriving show { with_path = false }]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let coord =
    lift3
      (fun x y z -> (x + 1, y + 1, z + 1))
      (integer <* char ',')
      (integer <* char ',')
      (integer <* end_of_line)

  let input = many coord
end

module Solving = struct
  open Base

  type cell = Air | Lava | Water

  let find_dims cubes =
    List.fold cubes ~init:(0, 0, 0) ~f:(fun (mx, my, mz) (x, y, z) ->
        (max mx (x + 2), max my (y + 2), max mz (z + 2)))

  let set map cell (x, y, z) = map.(x).(y).(z) <- cell

  let in_range (mx, my, mz) (x, y, z) =
    x >= 0 && x < mx && y >= 0 && y < my && z >= 0 && z < mz

  let is_cell map cell (x, y, z) = Poly.(map.(x).(y).(z) = cell)

  let get_adjacents =
    let d =
      [ (1, 0, 0); (-1, 0, 0); (0, 1, 0); (0, -1, 0); (0, 0, 1); (0, 0, -1) ]
    in
    let sum (x, y, z) (x', y', z') = (x + x', y + y', z + z') in
    fun p -> List.map d ~f:(sum p)

  let init_map (dimx, dimy, dimz) lava =
    let map =
      Array.init dimx ~f:(fun _ -> Array.make_matrix ~dimx:dimy ~dimy:dimz Air)
    in
    List.iter lava ~f:(set map Lava);
    map

  let count_surface map cell p =
    get_adjacents p |> List.count ~f:(is_cell map cell)

  let fill_cell queue map dims p =
    if in_range dims p && is_cell map Air p then (
      Queue.enqueue queue p;
      set map Water p)

  let fill_map map dims =
    set map Water (0, 0, 0);
    let todo = Queue.singleton (0, 0, 0) in
    let rec aux () =
      match Queue.dequeue todo with
      | None -> ()
      | Some current ->
          aux @@ List.iter (get_adjacents current) ~f:(fill_cell todo map dims)
    in
    aux ()

  let part1 (input : input) : output =
    let dims = find_dims input in
    let map = init_map dims input in
    List.sum (module Int) input ~f:(count_surface map Air)

  let part2 (input : input) : output =
    let dims = find_dims input in
    let map = init_map dims input in
    fill_map map dims;
    List.sum (module Int) input ~f:(count_surface map Water)
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
