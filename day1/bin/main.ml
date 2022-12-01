open Base

let parse_file data =
  let rec parse_elfs todo acc elfs =
    match todo with
    | [] -> acc::elfs
    | "" :: tl -> parse_elfs tl 0 (acc::elfs)
    | hd :: tl -> parse_elfs tl ((Int.of_string hd) + acc) elfs
  in
  parse_elfs data 0 []

let solve1 data = List.max_elt ~compare data |> Option.value ~default:0

let solve2 data =
  match List.sort ~compare data |> List.rev with
  | [] -> 0
  | elf1 :: [] -> elf1
  | elf1 :: elf2 :: [] -> elf1 + elf2
  | elf1 :: elf2 :: elf3 :: _ -> elf1 + elf2 + elf3

let main file =
  let data = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "File : %s\n\tPart 1 : %d\n\tPart 2 : %d\n"
    file (solve1 data) (solve2 data)

let _ = main "example.txt";  main "input.txt"