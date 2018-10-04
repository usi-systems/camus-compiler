open Core
open Query_Types

let rec get_l_idx func l =
    match l with
    | h::t -> if func h then 0 else 1 + get_l_idx func t
    | [] -> raise (Failure "Not Found")

module QuerySpec = struct
  type required_match =
    | ExactMatch
    | RangeMatch
    | ExactAndRangeMatch
    [@@deriving compare, sexp]

  type field_width = int [@@deriving compare, sexp]

  type field =
    | HeaderField of string * string * required_match * field_width
    [@@deriving compare, sexp]

  type counter = string * int
    [@@deriving compare, sexp]

  type t = {
             fields : field list;
             counters: counter list}
           [@@deriving compare, sexp]

  let field_priority t h f =
    if h="stful_meta" then
      1000 + (get_l_idx
        (fun (id, _) -> id=f) t.counters)
    else
      get_l_idx
        (fun field ->
          match field with
          | HeaderField(h2, f2, _, _) ->
            h=h2 && f=f2)
        t.fields

  let field_width t h f =
    let rec findrec l =
      match l with
      | HeaderField(h2, f2, _, w)::_ when h=h2 && f=f2 -> w
      | _::tail -> findrec tail
      | [] -> raise (Failure "Could not find width for header field")
    in
    findrec t.fields

  let prioritize_fields (qs:t) (rules:QueryRule.t list) =
    let open QueryFormula in
    let open AtomicPredicate in
    let getp h f =
      field_priority qs h f
    in
    let getw h f =
      field_width qs h f
    in
    let update_atom a =
      match a with
      | Eq(HeaderField(h, f, _, _), c) -> Eq(HeaderField(h, f, getp h f, getw h f), c)
      | Lt(HeaderField(h, f, _, _), c) -> Lt(HeaderField(h, f, getp h f, getw h f), c)
      | Gt(HeaderField(h, f, _, _), c) -> Gt(HeaderField(h, f, getp h f, getw h f), c)
    in
    let rec update_form q =
      match q with
      | Atom(a) -> Atom(update_atom a)
      | Not(x) -> Not(update_form x)
      | Or(x, y) -> Or(update_form x, update_form y)
      | And(x, y) -> And(update_form x, update_form y)
      | x -> x
    in
    Caml.List.map
      (fun (q, a) ->
        (update_form q, a))
      rules

end


let parse_query_ann ann_str =
  let ann_str = String.drop_suffix ann_str 0 in
  let lexbuf = Lexing.from_string ann_str in
  try
    `OK (Query_Annotation_Parser.query_stmt Query_Annotation_Lexer.token lexbuf)
  with
  | Query_Annotation_Lexer.LexerError _  ->
    `Error (Format.sprintf "no such token")
  | Query_Annotation_Parser.Error ->
    `Error (Format.sprintf "syntax error")
  | _ ->
     `Error (Format.sprintf "no such token")

let find_query_anns2 (p4_path:string) : query_annotation list =
  let lines = In_channel.read_lines p4_path in
  List.fold_left
    lines
    ~init:[]
    ~f:(fun l line ->
      if String.is_prefix line "@pragma query_" then (
            let ann_str = String.drop_prefix line 7 in
            match parse_query_ann ann_str with
            | `OK ann -> ann::l
            | `Error s -> Format.eprintf "%s\n%!" s; l) else l)

module FieldMap = Map.Make(struct type t = string * string [@@deriving compare, sexp] end)


let load_query_spec (p4_path:string) : QuerySpec.t =
  let open QuerySpec in
  let anns = find_query_anns2 p4_path in
  let fs =
    List.fold_left
    anns
    ~init:[]
    ~f:(fun l a ->
        match a with
        | QueryField(h, f, w) ->
            (HeaderField(h, f, ExactAndRangeMatch, w))::l
        | QueryFieldExact(h, f, w) ->
            (HeaderField(h, f, ExactMatch, w))::l
        | QueryFieldRange(h, f, w) ->
            (HeaderField(h, f, RangeMatch, w))::l
        | QueryFieldCounter(id, tumble_time) ->
            (HeaderField("stful_meta", id, RangeMatch, 16))::l
        | _ -> l) in
  let counters = [] in
  { fields = fs; counters = counters }



let print_fields (qs:QuerySpec.t) =
  List.iter
    qs.fields
    ~f:(fun f ->
        match f with
        | HeaderField(h, f, _, _) -> Format.eprintf "%s.%s\n" h f);
