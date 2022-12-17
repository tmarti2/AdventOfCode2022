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

  let create_shape top shape =
    List.map shape ~f:(fun (x,y) -> x, y + top - 4)

  let update map shape top =
    List.iter shape ~f:(fun (x,y) -> map.(y).(x) <- Full);
    get_top top shape

  let do_steps map dims top actions max_step =
    let rec do_step actions_left nb_steps top shape shape_id =
      if nb_steps = 0 then top
      else begin
        match actions_left, shape with
        | [], _ -> do_step actions nb_steps top shape shape_id
        | action :: left, Some shape ->
          begin match do_move map dims action shape with
            | shape, false ->
              let top = update map shape top in
              do_step left (nb_steps-1) top None (shape_id+1)
            | shape, true ->
              do_step left nb_steps top (Some shape) shape_id
          end
        | _, None ->
          let id = shape_id % 5 in
          let shape = create_shape top shapes.(id) in
          do_step actions_left nb_steps top (Some shape) id
      end
    in
    do_step actions max_step top None 0

  let part1 (input : input) : output =
    let dimx, dimy = 7, 10000 in
    let map = Array.make_matrix ~dimx:dimy ~dimy:dimx Empty in
    dimy - do_steps map (dimx, dimy) dimy input 2022

  let part2 (_input : input) : output =
    Stdlib.Format.printf "TODO P2@.%!"; 0
end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
