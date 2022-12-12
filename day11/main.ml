open Aoclib

module Types = struct

  type monkey = {
    mutable worries : int list;
    inspect : int -> int;
    throw : int -> int;
    mutable activity: int;
  } [@@deriving show {with_path = false}]

  type input = monkey list [@@deriving show]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let start =
    let id = string "Monkey " *> integer *> char ':' *> end_of_line in
    let worries = sep_by1 (string ", ") integer in
    id *> string "  Starting items: " *> worries <* end_of_line

  let inspect =
    let op =
      let v = (string "old" *> return `Old) <|> (integer >>| fun i -> `Int i) in
      let op = string " + " *> return (+) <|> string " * " *> return ( * ) in
      let f a x = match a with `Old -> x | `Int n -> n in
      lift3 (fun a op b x -> op (f a x) (f b x)) v op v
    in string "  Operation: new = " *> op <* end_of_line

  let throw =
    let get_i s = string s *> integer <* end_of_line in
    let test = get_i "  Test: divisible by " in
    let restrue = get_i "    If true: throw to monkey " in
    let resfalse = get_i "    If false: throw to monkey " in
    lift3 (fun test yes no ->
      fun v -> if v mod test = 0 then yes else no
      ) test restrue resfalse

  let monkey =
    lift3 (fun worries inspect throw ->
        {worries; inspect; throw; activity=0}
      ) start inspect throw
  let input = sep_by1 end_of_line monkey

end

module Solving = struct
  open Base

  let do_monkey ~f monkeys m =
    List.iter m.worries ~f:(fun item ->
        let new_worry = f (m.inspect item) in
        let next = m.throw new_worry in
        monkeys.(next).worries <- new_worry :: monkeys.(next).worries;
        m.activity <- m.activity +1
      );
    m.worries <- []

  let round ~f monkeys =
    Array.iter monkeys ~f:(do_monkey ~f monkeys)

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
