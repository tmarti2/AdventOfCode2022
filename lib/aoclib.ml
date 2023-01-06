module type Types = sig
  type input
  type output

  val pp_input : Format.formatter -> input -> unit
  val pp_output : Format.formatter -> output -> unit
end

module type Parsing = sig
  type input

  val input : input Angstrom.t
end

module type Solving = sig
  type input
  type output

  val part1 : input -> output
  val part2 : input -> output
end

module Parsing = struct
  open Angstrom

  let number = Base.Char.is_digit
  let letter = Base.Char.is_alpha
  let space = char ' '
  let integer = take_while1 number >>| int_of_string

  let neg_integer =
    lift2
      (fun sign n -> if sign = '-' then -n else n)
      (Angstrom.option '+' (char '-'))
      integer

  let word = take_while1 letter

  (** [enclosed l p r] creates a parser [char l *> p <* char r]  *)
  let enclosed l p r = char l *> p <* char r
end

module MakeDay
    (T : Types)
    (P : Parsing with type input := T.input)
    (S : Solving with type input := T.input and type output := T.output) =
struct
  open Cmdliner

  let do_parse str =
    let open Angstrom in
    let open Buffered in
    let state = parse P.input in
    let state = feed state (`String str) in
    let end_state = feed state `Eof in
    let result = state_to_result end_state in
    let unconsumed =
      match state_to_unconsumed end_state with
      | None -> None
      | Some { buf; off; len } ->
          if len = 0 then None else Some (Bigstringaf.substring buf ~off ~len)
    in
    let result =
      match (result, unconsumed) with
      | x, None -> x
      | Ok _, Some u -> Error (Printf.sprintf "unconsumed: '%s'" u)
      | Error msg, Some u ->
          Error (Printf.sprintf "%s / unconsumed: '%s'" msg u)
    in
    match result with Ok v -> v | Error msg -> failwith msg

  let run debug =
    let aux ?(debug = false) file =
      Format.printf "@.%!";
      Format.printf "File  : %s@.%!" file;
      let input = Stdio.In_channel.read_all file |> do_parse in
      if debug then Format.printf "Parsed:@[%a@]@.%!" T.pp_input input;
      let t1 = Unix.gettimeofday () in
      let o1 = S.part1 input in
      let t2 = Unix.gettimeofday () in
      let o2 = S.part2 input in
      let t3 = Unix.gettimeofday () in
      Format.printf "Part 1: %a in %fs@.%!" T.pp_output o1 (t2 -. t1);
      Format.printf "Part 2: %a in %fs@.%!" T.pp_output o2 (t3 -. t2)
    in
    aux ~debug "example.txt";
    if not debug then aux "input.txt"

  let debug_arg =
    Arg.info [ "d"; "debug" ] ~doc:"Enable debug mode" |> Arg.flag |> Arg.value

  let run_term = Term.(const run $ debug_arg)

  let cmd =
    let doc = "Advent Of Code library" in
    let man = [ `S Manpage.s_bugs ] in
    let info = Cmd.info "AOC-lib" ~doc ~man in
    Cmd.v info run_term

  let run_all () = exit (Cmd.eval cmd)
end

(* Compose *)
let ( << ) f g x = f (g x)
