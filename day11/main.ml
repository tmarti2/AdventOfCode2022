open Aoclib

module Types = struct

  type monkey = {
    worries : int list;
    inspect : int -> int;
    throw : int -> int;
    activity: int;
  } [@@deriving show {with_path = false}]

  type input = monkey list [@@deriving show]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let id = string "Monkey " *> integer <* char ':' <* end_of_line
  let worries = sep_by (string ", ") integer
  let start = string "  Starting items: " *> worries <* end_of_line

  let old = string "old" *> return `Old
  let int = integer >>| fun i -> `Int i
  let add = string " + " *> return (+)
  let mul = string " * " *> return ( * )
  let op =
    lift3 (fun a op b ->
        match a, b with
        | `Old, `Old -> fun a -> op a a
        | `Old, `Int i -> fun a -> op a i
        | `Int i, `Old -> fun a -> op i a
        | `Int i, `Int i' -> fun _ -> op i i'
      ) (int <|> old) (add <|> mul) (int <|> old)
  let inspect = string "  Operation: new = " *> op <* end_of_line

  let test = string "  Test: divisible by " *> integer <* end_of_line
  let restrue = string "    If true: throw to monkey " *> integer <* end_of_line
  let resfalse = string "    If false: throw to monkey " *> integer <* end_of_line
  let throw =
    lift3 (fun test yes no ->
      fun v -> if v mod test = 0 then yes else no
      ) test restrue resfalse

  let monkey = id *>
  lift3 (fun worries inspect throw ->
          {worries; inspect; throw; activity=0}
    ) start inspect throw
  let input = sep_by end_of_line monkey

end

module Solving = struct
  open Base

  let do_monkey ~f monkeys id monkey =
    let activity =
      List.fold ~init:monkey.activity monkey.worries ~f:(fun acc item ->
          let new_worry = f (monkey.inspect item) in
          let next = monkey.throw new_worry in
          monkeys.(next) <- {monkeys.(next) with worries = monkeys.(next).worries @ [new_worry]};
          acc + 1
        )
    in monkeys.(id) <- {monkey with activity; worries = []}

  let round ~f monkeys =
    Array.iteri monkeys ~f:(do_monkey ~f monkeys)

  let rec rounds ~f monkeys n =
    round ~f monkeys;
    if n <= 1 then () else rounds ~f monkeys (n-1)

  let business monkeys =
    Array.fold monkeys ~init:[] ~f:(fun acc m -> m.activity :: acc)
    |> List.sort ~compare:(Fn.flip compare)
    |> Fn.flip List.take 2
    |> List.reduce_exn ~f:( * )

  let n = 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23

  let part1 (input : input) : output =
    let monkeys = Array.of_list input in
    rounds monkeys 20 ~f:(fun a -> (a / 3) % n);
    business monkeys

  let part2 (input : input) : output =
    let monkeys = Array.of_list input in
    rounds monkeys 10000 ~f:(fun a -> a % n);
    business monkeys

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
