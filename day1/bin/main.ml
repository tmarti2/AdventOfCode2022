let parse_file data =
  let rec aux acc elfs =
    try
      match input_line data |> int_of_string_opt with
      | None -> aux 0 (acc::elfs)
      | Some x -> aux (acc+x) elfs
    with
      End_of_file -> acc::elfs
  in
  aux 0 []

let solve1 data = List.fold_left max 0 data

let solve2 data =
  match List.sort compare data |> List.rev with
  | [] -> 0
  | elf1 :: [] -> elf1
  | elf1 :: elf2 :: [] -> elf1 + elf2
  | elf1 :: elf2 :: elf3 :: _ -> elf1 + elf2 + elf3

let main file =
  let data = open_in file |> parse_file in
  Printf.printf "File : %s\n\tPart 1 : %d\n\tPart 2 : %d\n"
    file (solve1 data) (solve2 data)

let _ = main "example.txt";  main "input.txt"