open Aoclib

module Types = struct
  type dir = Right | Left | Down [@@deriving show {with_path = false}, sexp_of]
  type input = dir list [@@deriving show]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom

  let left = char '<' *> return Left
  let right = char '>' *> return Right
  let input = many1 (left <|> right) <* end_of_line

end

module Solving = struct
  open Base

  let hbar = [(2,0);(3,0);(4,0);(5,0)]
  let vbar = [(2,0);(2,-1);(2,-2);(2,-3)]
  let square = [(2,0);(3,0);(2,-1);(3,-1)]
  let plus = [(3,0);(2,-1);(3,-1);(4,-1);(3,-2);]
  let angle = [(2,0);(3,0);(4,0);(4,-1);(4,-2)]
  let shapes = [|hbar;plus;angle;vbar;square|]

  type cell = Empty | Full
  let _print_cell = function Empty -> "." | Full -> "#"

  let free map (dimx, dimy) (x, y) =
    x >= 0 && x < dimx && y >= 0 && y < dimy
    && Poly.(map.(y).(x) <> Full)

  let aux_move map dims dir shape =
    let (dx,dy) =
      match dir with
      | Left -> (-1,0)
      | Right -> (1,0)
      | Down -> (0,1)
    in
    let new_shape = List.map shape ~f:(fun (px,py) -> px+dx,py+dy) in
    if List.for_all new_shape ~f:(free map dims) then
      Some new_shape
    else None

  let rec do_move map dims dir shape =
    let ns = aux_move map dims dir shape in
    match dir, ns with
    | Left, Some ns | Right, Some ns -> do_move map dims Down ns
    | Left, None | Right, None -> do_move map dims Down shape
    | Down, None -> shape, false
    | Down, Some ns -> ns, true

  let get_top top shape =
    List.fold shape ~init:top ~f:(fun acc (_,y) -> min acc y)

  let get_top_grid map top size =
    let top = min top (Array.length map - size) in
    let l = ref[] in
    for y = top to top + size - 1 do
      l := Array.to_list map.(y) :: !l;
    done;
    List.rev !l

  let create_shape top shape =
    List.map shape ~f:(fun (x,y) -> x, y + top - 4)

  let update map shape top =
    List.iter shape ~f:(fun (x,y) -> map.(y).(x) <- Full);
    get_top top shape

  module Key = struct
    type t = int*int [@@deriving sexp_of]
    let compare = Poly.compare
    let hash = Hashtbl.hash
  end

  let use_cache cache first map cycle max_cycle new_size_tower top_tower key =
    let normal_way = (cycle + 1), new_size_tower in
    match !first, Hashtbl.find cache key with
    | false, _ -> normal_way
    | true, None ->
      let top_tower = get_top_grid map top_tower 39 in
      Hashtbl.add_exn cache ~key ~data:(top_tower, cycle, new_size_tower);
      normal_way
    | true, Some (old_top_tower, old_cycle, old_size_tower) ->
      let top_tower = get_top_grid map top_tower 39 in
      if Poly.(old_top_tower <> top_tower) then
        normal_way
      else begin
        first := false;
        let period = cycle - old_cycle in
        let height = new_size_tower - old_size_tower in
        let nb_app = (max_cycle - cycle) / period in
        (cycle+nb_app*period), (new_size_tower + (height*nb_app))
      end

  let do_steps map dims top actions max_step =
    let cache = Hashtbl.create ~growth_allowed:true ~size:10 (module Key) in
    let first = ref true in
    let rec do_step actions_left nb_steps top size_tower shape shape_id =
      if nb_steps = max_step then size_tower
      else begin
        match actions_left, shape with
        | [], _ -> do_step actions nb_steps top size_tower shape shape_id
        | action :: left, Some shape ->
          begin match do_move map dims action shape with
            | shape, false ->
              let top' = update map shape top in
              let ofst = top - top' in
              let new_size = size_tower + ofst in
              let key = (List.length left, shape_id + 1) in
              let cycle, size =
                use_cache cache first map nb_steps max_step new_size top' key
              in
              do_step left cycle top' size None ((shape_id+1)%5)
            | shape, true ->
              do_step left nb_steps top size_tower (Some shape) shape_id
          end
        | _, None ->
          let shape = create_shape top shapes.(shape_id) in
          do_step actions_left nb_steps top size_tower (Some shape) shape_id
      end
    in
    do_step actions 1 top 0 None 0

  let part1 (input : input) : output =
    let dimx, dimy = 7, 10000 in
    let map = Array.make_matrix ~dimx:dimy ~dimy:dimx Empty in
    do_steps map (dimx, dimy) dimy input 2022

  let part2 (input : input) : output =
    let dimx, dimy = 7, 10000 in
    let map = Array.make_matrix ~dimx:dimy ~dimy:dimx Empty in
    do_steps map (dimx, dimy) dimy input 1000000000000
end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
