open Core
open Query_Formula
open Query_Util

(* Parser and surface language types *)
type info = string * (int * int) * (int * int)

type query_annotation =
  | QueryControl
  | QueryField of string * string * int
  | QueryFieldExact of string * string * int
  | QueryFieldRange of string * string * int
  | QueryFieldCounter of string * int


module QueryConst = struct
  type t =
    | Number of int
    | IP of int
    | String of string
    [@@deriving compare, sexp]

  let format_t t =
    match t with
    | String s -> s
    | Number(n) ->
        string_of_int n
    | IP(i) ->
        Printf.sprintf "%d.%d.%d.%d"
          ((i lsr 24) land 255)
          ((i lsr 16) land 255)
          ((i lsr 8)  land 255)
          (i land 255)

  let compare a b =
    match (a, b) with
    | (((Number x)|(IP x)), ((Number y)|(IP y))) ->
        Pervasives.compare x y
    | (((Number _)|(IP _)), String s) -> 1
    | (String s, ((Number _)|(IP _))) -> -1
    | (String s1, String s2) ->
        Pervasives.compare s1 s2

  let min (a:t) (b:t) : t =
    if compare a b < 0 then a else b

  let max (a:t) (b:t) : t =
    if compare a b > 0 then b else a
end


module QueryField = struct
  type priority = int [@@deriving compare, sexp]
  type width = int [@@deriving compare, sexp]

  type t =
    | HeaderField of string * string * priority * width
    [@@deriving compare, sexp]

  let format_t t =
    match t with
    | HeaderField(h, f, _, _) -> Printf.sprintf "%s.%s" h f

  let compare a b =
    let apr =
      match a with
      | HeaderField(_, _, pr, _) -> pr in
    let bpr =
      match b with
      | HeaderField(_, _, pr, _) -> pr in
    Int.compare apr bpr

end

module AssignmentMap = Map.Make(QueryField)

module AtomicPredicate = struct
  type t =
    | Eq of QueryField.t * QueryConst.t
    | Lt of QueryField.t * QueryConst.t
    | Gt of QueryField.t * QueryConst.t
    [@@deriving compare, sexp]

  type assignments = QueryConst.t AssignmentMap.t

  let compare a b =
    match (a, b) with
    | (Eq(x, String s1), Eq(y, String s2)) when x=y -> String.compare s1 s2
    | (Gt(x, Number n1), Gt(y, Number n2)) when x=y -> Int.compare n2 n1
    | (Lt(x, Number n1), Lt(y, Number n2)) when x=y -> Int.compare n1 n2
    | (Eq(x, Number n1), Eq(y, Number n2)) when x=y -> Int.compare n1 n2
    (* Lt < Gt < Eq *)
    | (Eq(x, _), Lt(y, _)) when x=y -> 1
    | (Eq(x, _), Gt(y, _)) when x=y -> 1
    | (Lt(x, _), Eq(y, _)) when x=y -> -1
    | (Lt(x, _), Gt(y, _)) when x=y -> -1
    | (Gt(x, _), Eq(y, _)) when x=y -> -1
    | (Gt(x, _), Lt(y, _)) when x=y -> 1
    | ((Eq(x, _) | Lt(x, _) | Gt(x, _)), (Eq(y, _) | Lt(y, _) | Gt(y, _))) ->
        QueryField.compare x y

  let format_t t =
    match t with
    | Lt(qf, c) -> Printf.sprintf "%s<%s" (QueryField.format_t qf) (QueryConst.format_t c)
    | Gt(qf, c) -> Printf.sprintf "%s>%s" (QueryField.format_t qf) (QueryConst.format_t c)
    | Eq(qf, c) -> Printf.sprintf "%s=%s" (QueryField.format_t qf) (QueryConst.format_t c)

  let disjoint t1 t2 =
    match (t1, t2) with
    | (Eq(a, x), Eq(b, y)) when a=b -> x<>y
    | (Gt(a, Number x), Eq(b, Number y)) when a=b -> y<=x
    | (Eq(b, Number y), Gt(a, Number x)) when a=b -> y<=x
    | (Lt(a, Number x), Eq(b, Number y)) when a=b -> y>=x
    | (Eq(b, Number y), Lt(a, Number x)) when a=b -> y>=x
    | (Lt(a, Number x), Gt(b, Number y)) when a=b -> x<=(y+1)
    | (Gt(b, Number y), Lt(a, Number x)) when a=b -> x<=(y+1)
    | _ -> false

  let subset sub sup =
    match (sub, sup) with
    | (Gt(a, Number x), Gt(b, Number y)) when a=b -> x>=y
    | (Lt(a, Number x), Lt(b, Number y)) when a=b -> x<=y
    | (Eq(a, Number x), Gt(b, Number y)) when a=b -> x>y
    | (Eq(a, Number x), Lt(b, Number y)) when a=b -> x<y
    | _ -> false

  let field t =
    match t with
    | Lt(qf, _) | Gt(qf, _) | Eq(qf, _) -> qf

  let independent t1 t2 =
    field t1 <> field t2

  let equal a b =
    compare a b = 0

  let hash x =
    Hashtbl.hash x

  let eval (a:assignments) (p:t) =
    let f = field p in
    let v = AssignmentMap.find_exn a f in
    match p, v with
    | Eq(_, Number x), Number y -> y = x
    | Gt(_, Number x), Number y -> y > x
    | Lt(_, Number x), Number y -> y < x
    | Eq(_, String x), String y -> y = x
    | _ -> raise (Failure "Invalid assignment")

end

module QueryFormula = Formula(AtomicPredicate)

let format_int_list l =
  String.concat ~sep:"," (List.map ~f:string_of_int l)

module QueryAction = struct
  type t =
    | ForwardPort of int
    | P4Action of string * int list
    [@@deriving compare, sexp]

  let format_t t =
    match t with
    | ForwardPort(p) -> string_of_int p
    | P4Action(name, args) -> Printf.sprintf "%s(%s)" name (format_int_list args)

end

module QueryRule = struct
  type t =
    QueryFormula.t * QueryAction.t list
    [@@deriving compare, sexp]

  let format_t (q, acts) =
    Printf.sprintf "%s: %s;" (QueryFormula.format_t q) (format_list QueryAction.format_t acts)

  let from_ast (rl:Query_Ast.rule_list) : t list =
    let const_of_exp e =
      let open QueryConst in
      match e with
      | Query_Ast.NumberLit i -> Number i
      | Query_Ast.IpAddr i -> IP i
      | Query_Ast.StringLit s -> String s
      | _ -> raise (Failure "Should be a const value")
    in
    let str_of_exp e =
      let open QueryConst in
      match e with
      | Query_Ast.Field (_, f) -> f
      | _ -> raise (Failure "Should be a const string value")
    in

    let rec form_of_exp e =
      let open QueryFormula in
      let open AtomicPredicate in
      match e with
      | Query_Ast.Not(x) -> Not(form_of_exp x)
      | Query_Ast.And(x, y) -> And(form_of_exp x, form_of_exp y)
      | Query_Ast.Or(x, y) -> Or(form_of_exp x, form_of_exp y)
      | Query_Ast.Eq(Query_Ast.Field(h, f), y) ->
          let hf = QueryField.HeaderField(h, f, 0, 0) in
          Atom(AtomicPredicate.Eq(hf, const_of_exp y))
      | Query_Ast.Eq(Query_Ast.Call(func, f::_), y) ->
          assert (func <> "inc");
          let hf = QueryField.HeaderField("stful_meta", str_of_exp f, 0, 0) in
          Atom(AtomicPredicate.Eq(hf, const_of_exp y))
      | Query_Ast.Eq _ -> raise (Failure "Bad format for Eq")
      | Query_Ast.Lt(Query_Ast.Field(h, f), y) ->
          let hf = QueryField.HeaderField(h, f, 0, 0) in
          Atom(AtomicPredicate.Lt(hf, const_of_exp y))
      | Query_Ast.Lt(Query_Ast.Call(func, f::_), y) ->
          let hf = QueryField.HeaderField("stful_meta", str_of_exp f, 0, 0) in
          Atom(AtomicPredicate.Lt(hf, const_of_exp y))
      | Query_Ast.Lt _ -> raise (Failure "Bad format for Lt")
      | Query_Ast.Gt(Query_Ast.Field(h, f), y) ->
          let hf = QueryField.HeaderField(h, f, 0, 0) in
          Atom(AtomicPredicate.Gt(hf, const_of_exp y))
      | Query_Ast.Gt(Query_Ast.Call(func, f::_), y) ->
          let hf = QueryField.HeaderField("stful_meta", str_of_exp f, 0, 0) in
          Atom(AtomicPredicate.Gt(hf, const_of_exp y))
      | Query_Ast.Gt _ -> raise (Failure "Bad format for Gt")
      | Query_Ast.Call _ -> raise (Failure "Unsupported Call")
      | (Query_Ast.Field _) | (Query_Ast.StringLit _)
      | (Query_Ast.NumberLit _) | (Query_Ast.IpAddr _) ->
          raise (Failure "Unexpected value here")
    in

    let rec ints_of_lits (l:Query_Ast.expr list) : int list =
      match l with
      | [] -> []
      | Query_Ast.NumberLit(i)::t -> i::(ints_of_lits t)
      | _ -> raise (Failure "Must all be number literals")
    in

    let act_of_exp e =
      match e with
      | Query_Ast.Call("fwd", [Query_Ast.NumberLit(i)]) -> QueryAction.ForwardPort i
      | Query_Ast.Call("fwd", _) -> raise (Failure "Bad fwd action")
      | Query_Ast.Call(act, args) -> QueryAction.P4Action(act, (ints_of_lits args))
      | _ -> raise (Failure "Action must be of Call type, e.g. fwd(1)")
    in

    List.map
      rl
      ~f:(fun (q, al) ->
        (form_of_exp q,
        List.map ~f:act_of_exp al))
end
