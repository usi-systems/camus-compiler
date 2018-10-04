(* Parser and surface language types *)
type info = string * (int * int) * (int * int)

let str_of_info i =
  let fn, (l1, c1), (l2, c2) = i in
  if l1 = l2 then
    Printf.sprintf "File \"%s\", line %d, characters %d-%d"
      fn l1 c1 c2
  else
    Printf.sprintf "File \"%s\", line %d, character %d, to line %d, character %d"
      fn l1 c1 l2 c2

type expr =
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Call of string * expr list
  | Lt of expr * expr
  | Gt of expr * expr
  | Eq of expr * expr
  | Field of string * string
  | StringLit of string
  | NumberLit of int
  | IpAddr of int

type action_list = expr list

type query = expr

type rule = query * action_list

type rule_list = rule list

