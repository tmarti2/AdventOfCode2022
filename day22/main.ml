open Aoclib

module Types = struct

  type dir = Forward of int | RotateR | RotateL
  [@@deriving show {with_path = false}]
  type path = dir list [@@deriving show]

  type cell = Void | Air | Wall
  let pp_cell fmt = function
    | Void -> Format.pp_print_char fmt ' '
    | Air -> Format.pp_print_char fmt '.'
    | Wall -> Format.pp_print_char fmt '#'

  type map = cell list list [@@deriving show]
  let pp_map fmt a =
    Format.pp_print_newline fmt ();
    List.iter (fun row ->
        List.iter (fun c -> pp_cell fmt c;) row;
        Format.pp_print_newline fmt ()
      ) a

  type input = map * path [@@deriving show]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let forward = integer >>| fun i -> Forward i
  let rotate = char 'R' *> return RotateR
               <|> char 'L' *> return RotateL

  let path = many (forward <|> rotate) <* end_of_line

  let void = char ' ' *> return Void
  let air = char '.' *> return Air
  let wall = char '#' *> return Wall
  let row = many1 (void <|> air <|> wall)
  let map = many1 (row <* end_of_line)

  let input = both (map <* end_of_line) path
end

module Solving = struct
  open Base

  type dir = Up | Down | Left | Right

  let find_dims map =
    List.map map ~f:(fun l -> List.length l)
    |> List.max_elt ~compare |> Option.value_exn,
    List.length map

  let make_map (dimx, dimy) data =
    let map = Array.make_matrix ~dimx:dimy ~dimy:dimx Void in
    List.iteri data ~f:(fun y row ->
        List.iteri row ~f:(fun x c ->
            map.(y).(x) <- c
          );
        if List.length row < dimx then
          for x = List.length row to dimx - 1 do
            map.(y).(x) <- Void
          done
      );
    map

  let get_start map =
    fst @@ Array.findi_exn map.(0) ~f:(fun _ c ->
        match c with
        | Air -> true
        | _ -> false
      ), 0

  let rotate_left (pos,dir) =
    match dir with
    | Up -> pos,Left
    | Left -> pos,Down
    | Down -> pos,Right
    | Right -> pos,Up

  let rotate_right (pos,dir) =
    match dir with
    | Up -> pos,Right
    | Right -> pos,Down
    | Down -> pos,Left
    | Left -> pos,Up

  let get_delta = function
    | Up -> (0,-1)
    | Down -> (0,1)
    | Left -> (-1,0)
    | Right -> (1,0)

  let is_free map (x,y) = Poly.(map.(y).(x) = Air)

  let is_void map (x,y) = Poly.(map.(y).(x) = Void)

  let add (dimx, dimy) (x,y) (x',y') =
    (x + x') % dimx, (y + y') % dimy

  let move_until map dims (pos,dir) i =
    let d = get_delta dir in
    let rec aux last_valid current n =
      if n = 0 then last_valid, dir
      else
        let next = add dims current d in
        if is_free map next then aux next next (n-1)
        else if is_void map next then
          aux last_valid next n
        else last_valid, dir
    in aux pos pos i

  let do_action map dims pos = function
    | RotateL -> rotate_left pos
    | RotateR -> rotate_right pos
    | Forward i -> move_until map dims pos i

  let do_path map dims start path =
    let rec aux pos = function
      | [] -> pos
      | next :: todo ->
        aux (do_action map dims pos next) todo
    in
    aux start path

  let score =
    let score_dir = function
      | Up -> 3
      | Left -> 2
      | Down -> 1
      | Right -> 0
    in
    fun ((x,y),dir) ->
      (y+1) * 1000 + 4 * (x+1) + score_dir dir

  let part1 (input : input) : output =
    let map, path = input in
    let dims = find_dims map in
    let map = make_map dims map in
    let start = get_start map, Right in
    do_path map dims start path |> score

  let part2 (_input : input) : output =
    Stdlib.Format.printf "TODO p2@.%!"; 0

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
