open Aoclib

module Types = struct
  type tree =
  | File of string * int
  | Dir of string * int * tree list
  [@@deriving show]
  type input = tree [@@deriving show]

  type output = int [@@deriving show]
end
include Types
let tree = ref None
module Parsing = struct
  open Angstrom
  open Parsing
  open Base


  let curr = Stack.create ()
  let move = function
    | `Up -> ignore(Stack.pop_exn curr)
    | `Down d -> Stack.push curr d
  let path () = Stack.to_list curr |> List.rev |> String.concat ~sep:"/"

  let name = take_while (fun c -> not @@ Char.equal c '\n')

  let up = string ".." >>| (fun _ -> move `Up)
  let down = name >>| (fun d -> move @@ `Down d)
  let cd = string "$ cd " *> (up <|> down) <* end_of_line

  let add content =
    let dir = path () in
    let rec add_aux t content =
      match t with
      | File _ -> t
      | Dir (name,s,[]) when String.equal name dir -> Dir (name,s,content)
      | Dir (name,_,_) when String.equal name dir -> t
      | Dir (name,s,l) ->
        let nl =
          List.map ~f:(fun t -> add_aux t content) l
        in
        Dir (name,s,nl)
    in
    match !tree with
    | None -> tree := Some (Dir (dir,0,content))
    | Some t -> tree := Some (add_aux t content)

  let dir_size = function
    | File (_,s) -> s
    | Dir (_,s,_) -> s

  let rec compute_sizes tree =
    match tree with
    | File _ -> tree
    | Dir (name,_,content) ->
      let updated_content = List.map ~f:compute_sizes content in
      let s = List.sum (module Int) ~f:dir_size updated_content in
      Dir (name,s,updated_content)

  let dir = (string "dir " *> name <* end_of_line) >>| (fun n -> Dir (path()^"/"^n,0,[]))
  let file = lift2 (fun size name -> File(name,size)) integer (char ' ' *> name <* end_of_line)
  let ls = string "$ ls" *> end_of_line *> many (dir <|> file) >>| add
  let input = many (cd <|> ls) >>| (fun _ -> Option.value_exn !tree |> compute_sizes)
end

module Solving = struct

  let find f size tree =
    let rec aux acc t =
      match t with
      | File _ -> acc
      | Dir (_,s,content) ->
        let nacc = if f s size then s::acc else acc in
        List.fold_left (fun acc t ->
            aux acc t
          ) nacc content
    in
    aux [] tree

  let part1 (input : input) : output =
    tree := None;
    find (<=) 100000 input
    |> Base.(List.sum (module Int) ~f:Fn.id)

  let part2 (input : input) : output =
    let max, min = 70000000, 30000000 in
    let to_delete = min - (max - Parsing.dir_size input) in
    find (>=) to_delete input
    |> List.sort compare
    |> List.hd


end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
