open Aoclib

module Types = struct
  type tower = string Base.Stack.t

  let pp_tower fmt s =
    Base.Stack.to_list s |> Format.(pp_print_list pp_print_string fmt)

  let pp_array pp_elem fmt s =
    let pp_sep ppf () = Format.fprintf ppf "|" in
    Array.to_seq s |> Format.pp_print_seq ~pp_sep pp_elem fmt

  type stacks = tower array [@@deriving show]
  type action = { qty : int; src : int; dst : int } [@@deriving show]
  type input = stacks * action list [@@deriving show]
  type output = string [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing
  open Base

  let empty = string "   " >>| fun _ -> None
  let content = satisfy letter >>| Char.to_string >>| Option.some
  let crate = char '[' *> content <* char ']'
  let numbers = sep_by space (space *> integer <* space) <* end_of_line

  let add_to_stacks stacks l =
    List.iteri ~f:(fun i -> Option.iter ~f:(Stack.push stacks.(i))) l

  let create_stacks s =
    let nb_stacks = List.hd_exn s |> List.length in
    let stacks = Array.init nb_stacks ~f:(fun _ -> Stack.create ()) in
    List.iter (List.rev s) ~f:(add_to_stacks stacks);
    stacks

  let crates = sep_by space (empty <|> crate) <* end_of_line
  let stacks = many crates <* (numbers <* end_of_line) >>| create_stacks
  let action s = string s *> integer
  let create_action qty src dst = { qty; src; dst }

  let step =
    lift3 create_action (action "move ")
      (action " from " >>| Fn.flip ( - ) 1)
      (action " to " >>| Fn.flip ( - ) 1)

  let steps = many (step <* end_of_line)
  let input = both stacks steps
end

module Solving = struct
  open Base
  (* module Stack = Stdlib.Stack *)

  let collect s n =
    let aux acc = Stack.pop_exn s :: acc in
    Fn.apply_n_times ~n aux []

  let do_action rev stacks { qty; src; dst } =
    let removed = collect stacks.(src) qty in
    let removed = if rev then List.rev removed else removed in
    List.iter removed ~f:(Stack.push stacks.(dst))

  let copy_stacks stacks =
    Array.init (Array.length stacks) ~f:(fun i -> Stack.copy stacks.(i))

  let get_top stacks =
    Array.fold stacks ~init:"" ~f:(fun acc s -> acc ^ Stack.top_exn s)

  let aux_part ~rev (stacks, actions) =
    let copy = copy_stacks stacks in
    List.iter ~f:(do_action rev copy) actions;
    get_top copy

  let part1 (input : input) : output = aux_part ~rev:true input
  let part2 (input : input) : output = aux_part ~rev:false input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
