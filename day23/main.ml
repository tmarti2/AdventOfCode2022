open Aoclib

module Types = struct
  type cell = Empty | Elf | Next of int
  [@@deriving show {with_path = false}]

  type input = cell list list [@@deriving show]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom

  let empty = char '.' *> return Empty
  let elf = char '#' *> return Elf
  let row = many (empty <|> elf)
  let input = many (row  <* end_of_line)
end

module Solving = struct
  open Base

  type dir = N | S | W | E | NE | NW | SW | SE
  let moves = [|N;S;W;E|]

  let set map cell (ox,oy) = map.(oy).(ox) <- cell

  let find_dims map = 0, 0, List.length (List.hd_exn map), List.length map

  let find_inner map (_,_,mx,my) =
    Array.foldi map ~init:(mx,my, 0, 0) ~f:(fun y init row ->
        Array.foldi row ~init ~f:(fun x (minx,miny,maxx,maxy) c ->
            match c with
            | Elf -> (min minx x, min miny y, max maxx x, max maxy y)
            | _ ->  (minx,miny,maxx,maxy)
          )
      )

  let create_map data (_,_,mx,my) =
    let lxoffset = 30 in (* ugly af *)
    let rxoffset = 60 in
    let uyoffset = 15 in
    let dyoffset = 60 in
    let map = Array.make_matrix ~dimx:(my+uyoffset+dyoffset) ~dimy:(mx+lxoffset+rxoffset) Empty in
    List.iteri data ~f:(fun y row ->
        List.iteri row ~f:(fun x c ->
            set map c (x+lxoffset,y+uyoffset)
          )
      );
    map, (0,0,mx+lxoffset+rxoffset,my+uyoffset+dyoffset)

  let add (x,y) (x',y') = x + x', y + y'

  let get_offset = function
    | N -> (0,-1) | S -> (0,1)
    | W -> (-1,0) | E -> (1,0)
    | NE -> (1,-1)| NW -> (-1,-1)
    | SE -> (1,1) | SW -> (-1,1)

  let get_corners = function
    | N -> List.map [N;NE;NW] ~f:get_offset
    | S -> List.map [S;SE;SW] ~f:get_offset
    | W -> List.map [W;NW;SW] ~f:get_offset
    | E -> List.map [E;NE;SE] ~f:get_offset
    | _ -> assert false

  let find_elfs map =
    Array.foldi map ~init:[] ~f:(fun y init row ->
        Array.foldi row ~init ~f:(fun x acc c ->
            if Poly.(c = Elf) then (x,y) :: acc else acc
          )
      )

  let can_move =
    let moves_c = Array.map moves ~f:get_corners in
    fun map p i ->
      let nexts_pos = List.map moves_c.(i) ~f:(add p) in
      List.for_all nexts_pos ~f:(fun (x,y) ->
          match map.(y).(x) with
          | Empty | Next _ -> true
          | Elf -> false
        )

  let get_coord p dir = get_offset dir |> add p

  let has_neightboors map elf =
    List.exists [N;S;W;E;NW;NE;SE;SW] ~f:(fun d ->
        let (x,y) = get_coord elf d in
        Poly.(map.(y).(x) = Elf)
      )

  let update map (x,y) =
    map.(y).(x) <-
      match map.(y).(x) with
      | Empty -> Next 1
      | Next i -> Next (i + 1)
      | Elf -> assert false

  let move map i elf =
    let rec aux n =
      if n = 4 || not @@ has_neightboors map elf then None
      else
        let i = ((i + n) % 4) in
        match can_move map elf i, get_coord elf moves.(i) with
        | false, _ -> aux (n + 1)
        | true, next ->
          update map next;
          Some next
    in
    aux 0

  let round map elfs i =
    List.map elfs ~f:(move map i)
    |> List.map2_exn elfs ~f:(fun old_pos new_pos ->
        match new_pos with
        | None -> old_pos
        | Some (x,y) ->
          match map.(y).(x) with
          | Empty -> old_pos
          | Elf -> assert false
          | Next i when i = 1 ->
            set map Empty old_pos;
            set map Elf (x,y);
            (x,y)
          | Next _ ->
            set map Empty (x,y);
            old_pos
      )
  , (i + 1) % 4

  let rounds ?stop (map, _, elfs) =
    let rec aux i elfs n =
      if Option.is_some stop && Option.value_exn stop < n then n
      else begin
        let nelfs, ni = round map elfs i in
        if Poly.(nelfs = elfs) then n
        else aux ni nelfs (n + 1)
      end
    in
    aux 0 elfs 1

  let count_empty map (minx,miny,maxx,maxy) =
    let in_range (x,y) = x >= minx && x <= maxx && y >= miny && y <= maxy in
    Array.foldi map ~init:0 ~f:(fun y init row ->
        Array.foldi row ~init ~f:(fun x acc c ->
            if in_range (x,y) && Poly.(c = Empty) then acc + 1 else acc
          )
      )

  let get_data input =
    let dims = find_dims input in
    let map,dims = create_map input dims in
    let elfs = find_elfs map in
    map, dims, elfs

  let part1 (input : input) : output =
    let map, dims, elfs = get_data input in
    let _ = rounds ~stop:10 (map,dims,elfs) in
    count_empty map (find_inner map dims)

  let part2 (input : input) : output =
    get_data input |> rounds
end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
