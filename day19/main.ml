open Aoclib

module Types = struct
  open Base

  type blueprint = {
    pore : int;
    pclay : int;
    pobs_ore : int;
    pobs_clay : int;
    pgeode_ore : int;
    pgeode_obs : int;
  } [@@deriving show {with_path = false}]
  type input = blueprint list [@@deriving show]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let id = string "Blueprint " *> integer <* string ": "
  let rore = string "Each ore robot costs " *> integer <* string " ore. "
  let rclay = string "Each clay robot costs " *> integer <* string " ore. "
  let robs = both
      (string "Each obsidian robot costs " *> integer)
      ( string " ore and " *> integer <* string " clay. ")
  let rgeode = both
      (string "Each geode robot costs " *> integer)
      ( string " ore and " *> integer <* string " obsidian.")

  let blueprint =
    lift4
      (fun pore pclay (pobs_ore,pobs_clay) (pgeode_ore, pgeode_obs) ->
         {pore; pclay; pobs_ore; pobs_clay; pgeode_ore; pgeode_obs}
      ) rore rclay robs rgeode

  let input = many (id *> blueprint <* end_of_line)
end

module Solving = struct
  open Base

  type state = {
    ore : int ;
    clay : int;
    obs : int;
    geode : int;
    rore : int;
    rclay : int;
    robs : int;
    rgeode : int;
  }

  let init = {
    ore = 0;
    clay = 0;
    obs = 0;
    geode = 0;
    rore = 1;
    rclay = 0;
    robs = 0;
    rgeode = 0
  }

  type build  = Ore | Clay | Obs | Geode
  type action = Init | Build | Wait of state

  let incr st =
    {st with
     ore = st.ore + st.rore;
     clay = st.clay + st.rclay;
     obs = st.obs + st.robs;
     geode = st.geode + st.rgeode;}

  let can_build bp st = function
    | Ore -> st.ore >= bp.pore
    | Clay -> st.ore >= bp.pclay
    | Obs -> st.ore >= bp.pobs_ore && st.clay >= bp.pobs_clay
    | Geode -> st.ore >= bp.pgeode_ore && st.obs >= bp.pgeode_obs

  let update ?(ore=0) ?(clay=0) ?(obs=0) ?(rore=0) ?(rclay=0) ?(robs=0) ?(rgeode=0) st =
    {st with
     ore = st.ore - ore;
     clay = st.clay - clay;
     obs = st.obs - obs;
     rore = st.rore + rore;
     rclay = st.rclay + rclay;
     robs = st.robs + robs;
     rgeode = st.rgeode + rgeode;
    }

  let build_robot bp st r =
    match r with
    | Ore  -> update ~ore:bp.pore ~rore:1 st
    | Clay -> update ~ore:bp.pclay ~rclay:1 st
    | Obs  -> update ~ore:bp.pobs_ore ~clay:bp.pobs_clay ~robs:1 st
    | Geode -> update ~ore:bp.pgeode_ore ~obs:bp.pgeode_obs ~rgeode:1 st

  let do_actions bp st n actions =
    let next = incr st in
    let rec aux acc = function
      | [] -> acc
      | hd :: todo ->
        if can_build bp st hd then
          aux ((n-1,Build, build_robot bp next hd) :: acc) todo
        else aux acc todo
    in
    aux [(n-1, Wait st, next)] actions

  let step bp (ore, clay, obs) (n, prev, st) =
    if can_build bp st Geode then
      match prev with
      | Wait st' ->
        if can_build bp st' Geode then []
        else [n-1, Build, build_robot bp (incr st) Geode]
      | _ -> [n-1, Build, build_robot bp (incr st) Geode]
    else
      List.filter [Ore; Clay; Obs]
        ~f:(fun a ->
            match prev, a with
            | Wait st', Ore -> st.rore < ore && not @@ can_build bp st' Ore
            | Wait st', Clay -> st.rclay < clay && not @@ can_build bp st' Clay
            | Wait st', Obs -> st.robs < obs && not @@ can_build bp st' Obs
            | _, Ore -> st.rore < ore
            | _, Clay -> st.rclay < clay
            | _, Obs -> st.robs < obs
            | _ -> assert false
          )
      |> do_actions bp st n

  let n_step n bp =
    let max_ressources =
      max (max bp.pore bp.pclay) (max bp.pobs_ore bp.pgeode_ore),
      bp.pobs_clay,
      bp.pgeode_obs
    in
    let stack = Stack.singleton (n, Init, init) in
    let rec aux m =
      match Stack.pop stack with
      | None -> m
      | Some (0, _, st) -> aux (max m st.geode)
      | Some (n, prev, st) ->
        let m' = st.geode + st.rgeode * n + (n * (n-1) / 2) in
        if m' > m then
          step bp max_ressources (n, prev, st)
          |> List.iter ~f:(Stack.push stack);
        aux m
    in
    aux 0

  let part1 (input : input) : output =
    let quality max i bp =
      n_step max bp |> ( * ) (i + 1)
    in
    List.mapi input ~f:(quality 24)
    |> List.sum (module Int) ~f:Fn.id

  let part2 (input : input) : output =
    List.take input 3
    |> List.map ~f:(n_step 32)
    |> List.reduce_exn ~f:( * )

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
