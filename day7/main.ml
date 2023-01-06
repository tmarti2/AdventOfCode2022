open Aoclib

module Types = struct
  type kind = Dir of int option | File of int
  [@@deriving show { with_path = false }]

  type label = { name : string; kind : kind }
  [@@deriving show { with_path = false }]

  type action = GoRoot | GoUp | GoDown of string | Ls of label list
  [@@deriving show { with_path = false }]

  type input = action list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let name = take_while (fun c -> not @@ Char.equal c '\n')
  let up = (string "$ cd .." <* end_of_line) *> return GoUp
  let down = string "$ cd " *> name <* end_of_line >>| fun d -> GoDown d
  let root = (string "$ cd /" <* end_of_line) *> return GoRoot
  let cd = root <|> up <|> down
  let dir = string "dir " *> return (Dir None)
  let file = integer <* space >>| fun s -> File s
  let lab = lift2 (fun kind name -> { name; kind }) (dir <|> file) name
  let result = many (lab <* end_of_line) >>| fun res -> Ls res
  let ls = (string "$ ls" <* end_of_line) *> result
  let input = many (cd <|> ls)
end

module Solving = struct
  open Base

  type tree = { label : label; children : tree list }
  [@@deriving show { with_path = false }]

  type path = Root | Parent of path * tree

  let go_up (t, p) =
    match p with
    | Root -> None
    | Parent (path, parent) ->
        Some ({ parent with children = t :: parent.children }, path)

  let rec go_root (t, p) =
    match go_up (t, p) with None -> (t, p) | Some (t', p') -> go_root (t', p')

  let explore (t, p) d =
    let child, others =
      List.partition_tf t.children ~f:(fun c -> String.equal c.label.name d)
    in
    (List.hd_exn child, Parent (p, { t with children = others }))

  let add_info (t, p) ls =
    let make_node label = { label; children = [] } in
    let childs = List.map ~f:make_node ls in
    ({ t with children = childs }, p)

  let do_action zip = function
    | GoRoot -> go_root zip
    | GoUp -> go_up zip |> Option.value_exn
    | GoDown d -> explore zip d
    | Ls info -> add_info zip info

  let build_tree actions =
    let init =
      ({ label = { name = "/"; kind = Dir None }; children = [] }, Root)
    in
    List.fold ~init ~f:do_action actions |> go_root |> fst

  let size t =
    match t.label.kind with
    | File s -> s
    | Dir (Some s) -> s
    | Dir None -> assert false

  let rec compute_size t =
    match t.label.kind with
    | File _ | Dir (Some _) -> t
    | Dir None ->
        let children = List.map ~f:compute_size t.children in
        let size = List.sum (module Int) ~f:size children in
        { label = { t.label with kind = Dir (Some size) }; children }

  let rec tree_fold ~f ~init t =
    let init = f init t in
    List.fold t.children ~init ~f:(fun init c -> tree_fold ~init ~f c)

  let find_match op size t =
    let f acc t =
      match t.label.kind with
      | File _ | Dir None -> acc
      | Dir (Some s) -> if op s size then s :: acc else acc
    in
    tree_fold ~f ~init:[] t

  let part1 (input : input) : output =
    build_tree input |> compute_size |> find_match ( <= ) 100000
    |> List.sum (module Int) ~f:Fn.id

  let part2 (input : input) : output =
    let max, min = (70000000, 30000000) in
    let t = build_tree input |> compute_size in
    let to_delete = min - (max - size t) in
    find_match ( >= ) to_delete t |> List.min_elt ~compare |> Option.value_exn
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
