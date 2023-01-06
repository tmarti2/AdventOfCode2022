open Aoclib

module Types = struct
  type input = int list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input = many (neg_integer <* end_of_line)
end

module Solving = struct
  open Base

  let next_pos arr size n =
    let i, (_, e) = Array.findi_exn arr ~f:(fun _ (i, _) -> i = n) in
    (i, (i + e) % (size - 1), e)

  let loop ~f src dst =
    let step = if src <= dst then 1 else -1 in
    let rec aux current =
      if current = dst then f current
      else (
        f current;
        aux (current + step))
    in
    aux src

  let move_all arr size n =
    let rec aux n =
      if n = size then 0
      else
        let i, ni, e = next_pos arr size n in
        if ni < i then loop i (ni + 1) ~f:(fun j -> arr.(j) <- arr.(j - 1))
        else loop i ni ~f:(fun j -> arr.(j) <- arr.(j + 1));
        arr.(ni) <- (n, e);
        aux (n + 1)
    in
    Fn.apply_n_times ~n aux 0 |> ignore

  let solve input key mix =
    let data = List.mapi input ~f:(fun i e -> (i, e * key)) |> Array.of_list in
    let size = Array.length data in
    move_all data size mix;
    let zero_i, _ = Array.findi_exn data ~f:(fun _ (_, e) -> e = 0) in
    List.sum
      (module Int)
      [ 1000; 2000; 3000 ]
      ~f:(fun i -> snd data.((i + zero_i) % size))

  let part1 (input : input) : output = solve input 1 1
  let part2 (input : input) : output = solve input 811589153 10
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
