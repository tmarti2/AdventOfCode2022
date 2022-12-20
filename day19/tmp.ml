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
  } [@@deriving show {with_path = false}]

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

  let wait st =
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

  let build_robot bp st r =
    if not @@ can_build bp st r then None
    else
      let st = wait st in
      match r with
      | Ore  ->
        Some {st with ore = st.ore - bp.pore; rore = st.rore + 1}
      | Clay ->
        Some {st with ore = st.ore - bp.pclay; rclay = st.rclay + 1}
      | Obs  ->
        Some {st with ore = st.ore - bp.pobs_ore; clay = st.clay - bp.pobs_clay; robs = st.robs + 1}
      | Geode ->
        Some {st with ore = st.ore - bp.pgeode_ore; obs = st.obs - bp.pgeode_obs; rgeode = st.rgeode + 1}

  let do_actions bp n st actions =
    let rec aux acc = function
      | [] -> acc
      | hd :: todo ->
        match build_robot bp st hd with
        | Some st' -> aux ((n-1, Build, st') :: acc) todo
        | None -> aux acc todo
    in
    aux [(n-1, Wait st, wait st)] actions

  let step bp (ore, clay, obs) (n, prev, st) =
    match build_robot bp st Geode, prev with
    | Some st, Wait st' ->
      if not @@ can_build bp st' Geode
      then [(n-1, Build, st)] else []
    | Some st, _ -> [(n-1, Build, st)]
    | None, _ ->
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
      |> do_actions bp n st

  let n_step n bp =
    let max_ressources =
      max (max bp.pore bp.pclay) (max bp.pobs_ore bp.pgeode_ore),
      bp.pobs_clay,
      bp.pgeode_obs
    in
    let q = Queue.singleton (n, Init, init) in
    let rec aux m =
      match Queue.dequeue q with
      | None -> m
      | Some (n', prev, st) ->
        let m' = st.geode + st.rgeode * n' + (n' * (n'-1) / 2) in
        if m' <= m then aux m
        else if n' = 0 then
          max m m'
        else
          (step bp max_ressources (n', prev, st) |> Queue.enqueue_all q; aux m)
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
    |> List.map ~f:(n_step 0)
    |> List.reduce_exn ~f:( * )

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
