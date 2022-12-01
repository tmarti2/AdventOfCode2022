open Base

let parse_file data =
  let rec parse_elfs todo acc elfs =
    match todo with
    | [] -> acc::elfs
    | "" :: tl -> parse_elfs tl [] (acc::elfs)
    | hd :: tl -> parse_elfs tl ((Int.of_string hd)::acc) elfs
  in
  parse_elfs data [] []

let solve1 data =
  List.fold_left data ~init:0 ~f:(fun acc el ->
      let max = List.sum (module Int) el ~f:(fun i -> i) in
      if max > acc then max else acc
    )

let solve2 data =
  let all = List.fold_left data ~init:[]
      ~f:(fun acc el -> (List.sum (module Int) el ~f:(fun i -> i)) :: acc
    ) |> List.sort ~compare |> List.rev in
  match all with
  | [] -> 0
  | elf1 :: [] -> elf1
  | elf1 :: elf2 :: [] -> elf1 + elf2
  | elf1 :: elf2 :: elf3 :: _ -> elf1 + elf2 + elf3

let main file =
  let data = Stdio.In_channel.read_lines file |> parse_file in
  Stdio.printf "File : %s\n\tPart 1 : %d\n\tPart 2 : %d\n"
    file (solve1 data) (solve2 data)

let _ = main "example.txt";  main "input.txt"