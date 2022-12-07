open Aoclib

module Types = struct
  type tree =
  | File of string*int
  | Dir of string*int*tree list
  [@@deriving show]

  type action = Root | Up | Down of string | Ls of tree list
  [@@deriving show]

  type input = action list [@@deriving show]

  type output = int [@@deriving show]
end
include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let name = take_while (fun c -> not @@ Char.equal c '\n')

  let up = (string "$ cd .."  <* end_of_line) *> return Up
  let down = (string "$ cd " *> name  <* end_of_line) >>| fun d -> (Down d)
  let root =  (string "$ cd /" <* end_of_line) *> return Root
  let cd = root <|> up <|> down

  let dir = string "dir " *> name >>| fun d -> Dir(d,0,[])
  let file = lift2 (fun s n -> File(n,s)) integer (char ' ' *> name)
  let result =
    many ((dir <|> file) <* end_of_line) >>| (fun res -> Ls res)
  let ls = (string "$ ls" <* end_of_line) *> result

  let input = many (cd <|> ls)
end

module Solving = struct
  open Base

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

  let find f size tree =
    let rec aux acc t =
      match t with
      | File _ -> acc
      | Dir (_,s,content) ->
        let nacc = if f s size then s::acc else acc in
        List.fold ~init:nacc ~f:(fun acc t ->
            aux acc t
          ) content
    in
    aux [] tree

  let absolute_path p t = match t with
    | File (n,s) -> File (n,s)
    | Dir (d,s,c) ->
      if String.equal p "/" then Dir(p^d, s, c) else Dir(p^"/"^d, s, c)

  let construct actions =
    let pos = Stack.create () in
    let path () = Stack.to_list pos |> List.rev |> String.concat ~sep:"/" in
    let pname () = "/" ^ path () in
    let rec add l t =
      let curr = pname () in
      match t with
      | File _ -> t
      | Dir (d, s, []) when String.equal d curr -> Dir(d, s, l)
      | Dir (d, _, _) when String.equal d curr -> t
      | Dir (d, s, c) ->
        Dir (d, s, List.map ~f:(add l) c)
    in
    let rec aux todo t =
      match todo with
      | [] -> t
      | Root :: tl -> Stack.clear pos; aux tl t
      | Up :: tl -> ignore(Stack.pop_exn pos); aux tl t
      | Down d :: tl -> Stack.push pos d; aux tl t
      | Ls ls :: tl ->
        let ls = List.map ~f:(absolute_path (pname ())) ls in
        let t' = add ls t in
        aux tl t'
    in aux actions (Dir("/",0,[]))


  let part1 (input : input) : output =
    let build = construct input |> compute_sizes in
    find (<=) 100000 build
    |> Base.(List.sum (module Int) ~f:Fn.id)

  let part2 (input : input) : output =
    let build = construct input |> compute_sizes in
    let max, min = 70000000, 30000000 in
    let to_delete = min - (max - dir_size build) in
    find (>=) to_delete build
    |> List.sort ~compare
    |> List.hd_exn

end

module Today = MakeDay(Types)(Parsing)(Solving)

let () = Today.run_all ()
