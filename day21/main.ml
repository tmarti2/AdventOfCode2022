open Aoclib

module Types = struct

  type op = Add | Sub | Mul | Div [@@deriving show]
  type action = Int of int | Op of string * op * string
  [@@deriving show {with_path = false}]
  type monkey = string*action [@@deriving show]

  type input = monkey list [@@deriving show]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let add = string " + " *> return Add
  let sub = string " - " *> return Sub
  let mul = string " * " *> return Mul
  let div = string " / " *> return Div
  let op = add <|> sub <|> mul <|> div

  let name = word <* string ": "
  let int = integer >>| fun i -> Int i
  let action =
    int <|> lift3 (fun a op b -> Op(a,op,b)) word op word

  let monkey = both name action
  let input = many (monkey <* end_of_line)
end

module Solving = struct
  open Base

  type tree = Val of int | X | Node of tree * op * tree

  let get_op =
    function Add -> ( + ) | Sub -> ( - ) | Mul -> ( * ) | Div -> ( / )

  let inv_op =
    function Add -> ( - ) | Sub -> ( + ) | Mul -> ( / ) | Div -> ( * )

  let build_h monkeys =
    let h = Hashtbl.create (module String) ~size:(List.length monkeys) in
    List.iter monkeys ~f:(fun (name,action) ->
        Hashtbl.add_exn h ~key:name ~data:action
      );
    h

  let build_tree ?var monkeys=
    let h = build_h monkeys in
    let rec aux name =
      let current = Hashtbl.find_exn h name in
      match current, var with
      | Int _, Some name' when Poly.(name = name') -> X
      | Int i, _ -> Val i
      | Op (a, op, b), _ ->
        Node (aux a, op, aux b)
    in
    aux "root"

  let get = function
    | Val i -> i
    | _ -> assert false

  let simplify tree =
    let rec aux t =
      match t with
      | Val _ | X -> t
      | Node (a, op, b) ->
        match aux a, aux b with
        | Val a, Val b -> Val (get_op op a b)
        | a, b -> Node (a, op, b)
    in
    aux tree

  let solve equation =
    let rec aux left acc =
      match left with
      | X -> Val acc
      | Node (b, op, Val a)
      | Node (Val a, (Add | Mul as op), b) ->
        aux b (inv_op op acc a)
      | Node (Val a, (Sub | Div as op), b) ->
        aux b (get_op op a acc)
      | _ -> assert false
    in
    match equation with
    | Node (Val res, _, a)
    | Node (a, _, Val res) ->
      aux a res
    | _ -> assert false

  let part1 (input : input) : output =
    build_tree input |> simplify |> get

  let part2 (input : input) : output =
    build_tree ~var:"humn" input |> simplify |> solve |> get

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
