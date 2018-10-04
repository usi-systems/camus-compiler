open Query_Spec
open Re2
open Query_Formula
open Query_Util
open Query_Types
open Query_Bdd
open Core

let rep a b =
  Regex.replace_exn ~f:(fun _ -> b) (Regex.create_exn a)

let escape s =
  rep "<" "&lt;" (rep ">" "&gt;" s)

let escape_quote s =
  rep "\"" "\\\"" s

module QueryTable = struct
  type node_id = int [@@deriving compare, sexp]

  type state = int [@@deriving compare, sexp]

  type field_match =
    | LtMatch of QueryConst.t
    | GtMatch of QueryConst.t
    | EqMatch of QueryConst.t
    | RangeMatch of QueryConst.t * QueryConst.t
    | Wildcard
    [@@deriving compare, sexp]

  type entry =
    | Transition of state * field_match * state
    | Terminal of state * QueryAction.t list
    [@@deriving compare, sexp]

  module EntrySet = Set.Make(struct type t = entry [@@deriving compare, sexp] end)

  type t = {
    field: QueryField.t option;
    entries: entry list;
    is_terminal: bool;
    }
    [@@deriving compare, sexp]

  let cmp_match a b =
    match (a, b) with
    | (x, y) when x=y -> 0
    | (Wildcard, Wildcard) -> 0
    | (EqMatch x, EqMatch y) | (LtMatch x, LtMatch y) | (GtMatch y, GtMatch x) ->
        QueryConst.compare x y
    | (RangeMatch (x, y), RangeMatch (j, k)) ->
        let c = QueryConst.compare x j in
        if c = 0 then QueryConst.compare y k else c
    | (EqMatch _, _) -> -1
    | (_, EqMatch _) -> 1
    | (RangeMatch _, _) -> -1
    | (_, RangeMatch _) -> 1
    | (LtMatch _, _) -> -1
    | (_, LtMatch _) -> 1
    | (GtMatch _, _) -> -1
    | (_, GtMatch _) -> 1

  let cmp_entry a b =
    match (a, b) with
    | (Transition _, Terminal _) -> -1
    | (Terminal _, Transition _) -> 1
    | (Transition (s1, m1, _), Transition (s2, m2, _)) when s1 = s2 ->
        cmp_match m1 m2
    | (Transition (s1, _, _), Transition (s2, _, _)) ->
        Pervasives.compare s1 s2
    | (Terminal (s1, _), Terminal (s2, _)) -> Pervasives.compare s1 s2

  let sort_entries entries =
    List.sort
      ~cmp:cmp_entry
      entries

  let format_match m =
    match m with
    | EqMatch(c) -> Printf.sprintf "= %s" (QueryConst.format_t c)
    | LtMatch(c) -> Printf.sprintf "< %s" (QueryConst.format_t c)
    | GtMatch(c) -> Printf.sprintf "> %s" (QueryConst.format_t c)
    | RangeMatch(c1, c2) ->
        Printf.sprintf "%s -> %s" (QueryConst.format_t c1) (QueryConst.format_t c2)
    | Wildcard -> "*"

  let table_name t =
    if t.is_terminal then "actions"
    else (
      let open QueryField in
      match t.field with
      | Some (HeaderField(h, f, _, _)) -> h ^ "_" ^ f
      | None -> "__noname__"
    )

  let format_t t =
      (Printf.sprintf
        "table_%s [shape=none margin=0 label=<
        <table cellpadding=\"3\" cellspacing=\"0\" border=\"0\" cellborder=\"1\">
        <tr><td colspan=\"3\"><b>%s</b></td></tr>
        <tr><td bgcolor=\"gray\">in</td><td bgcolor=\"gray\">match</td><td bgcolor=\"gray\">out</td></tr>"
        (table_name t) (table_name t)) ^
      (Caml.String.concat "|"
        (List.map
          t.entries
          ~f:(fun e ->
            match e with
            | Transition(s1, m, s2) ->
               Printf.sprintf "<tr><td>%d</td><td>%s</td><td>%d</td></tr>"
                 s1 (escape (format_match m)) s2
            | Terminal(s, al) ->
                Printf.sprintf "<tr><td>%d</td><td></td><td>%s</td></tr>"
                  s (escape (format_list QueryAction.format_t al))))) ^

      "</table>>];\n"

end

module QueryTablePipeline = struct
  open QueryTable

  type t = {
    tables: QueryTable.t list;
    }
    [@@deriving compare, sexp]


  let to_dot bdd t =
   (Printf.sprintf "digraph %s {\n" "G") ^
   (QueryBdd.format_t ~graph_name:"subgraph" bdd ) ^
   "legend [shape=note style=filled label=\"" ^
     (* TODO: pretty print rules
   (Caml.List.fold_left (fun s (qf, al) -> s ^ (
      Printf.sprintf "%s: %s\\l"
                                    (escape_quote (QueryFormula.format_t qf))
                                    (format_list QueryBdd.Label.format_t al))) "" bdd.rules) ^
                                    *)
   "\"];\n" ^
   "subgraph {rank=same;\n" ^
    (Caml.String.concat "\n"
      (List.map t.tables ~f:QueryTable.format_t)) ^
    (Caml.String.concat "\n"
      (snd (List.fold_left
        t.tables
        ~init:(None, [])
        ~f:(fun x tbl ->
          let tn = QueryTable.table_name tbl in
          match x with
          | (None, _) -> (Some tn, [])
          | (Some prev, sl) ->
              (Some tn, (Printf.sprintf "table_%s -> table_%s;" prev tn)::sl))))) ^
   "}}\n"

  let create (qs:QuerySpec.t) (bdd:QueryBdd.t) =
    let open QueryBdd in

    (* XXX we re-number the root to state 0 here *)
    (match bdd.root with
      | L _ -> ()
      | N {uid=uid; var=v; low=l; high=h} ->
          bdd.root <- N {uid=0; var=v; low=l; high=h});

    (* Get a list of all atomic predicates *)
    let preds = QueryBdd.VarSet.to_list (QueryBdd.vars bdd) in

    (* Get all the fields used in the rules *)
    let fields =
      Caml.List.sort_uniq
        QueryField.compare
        (Caml.List.map
          AtomicPredicate.field
          preds) in

    (* Get all the terminal (action) states *)
    let terminal_states = QueryBdd.leaves bdd in

    let sorted_action_states =
      List.sort
       ~cmp:Pervasives.compare
       (NodeSet.elements terminal_states) in

    let actions_table = {
      field = None;
      is_terminal = true;
      entries =
        (List.map
          sorted_action_states
          ~f:(fun u -> Terminal (getuid u, LabelSet.to_list (getlabels u))));
      } in

    let make_match (pred:AtomicPredicate.t) : field_match =
      let open AtomicPredicate in
      match pred with
      | Lt (_, x) -> LtMatch x
      | Gt (_, x) -> GtMatch x
      | Eq (_, x) -> EqMatch x
    in

    (* This assumes that m and the new pred are overlapping *)
    let refine_match (m:field_match) (pred:AtomicPredicate.t) : field_match =
      let m2 = make_match pred in
      match m, m2 with
      | Wildcard, _ | _, Wildcard -> m2
      | EqMatch x, _ | _, EqMatch x -> EqMatch x
      | GtMatch x1, GtMatch x2 -> GtMatch (QueryConst.max x1 x2)
      | LtMatch y1, LtMatch y2 -> LtMatch (QueryConst.min y1 y2)
      | LtMatch x, GtMatch y | GtMatch y, LtMatch x -> RangeMatch (x, y)
      | RangeMatch (x1, y1), LtMatch y2 | LtMatch y2, RangeMatch (x1, y1) ->
          RangeMatch (x1, QueryConst.min y1 y2)
      | RangeMatch (x1, y1), GtMatch x2 | GtMatch x2, RangeMatch (x1, y1) ->
          RangeMatch (QueryConst.max x1 x2, y1)
      | RangeMatch (x1, y1), RangeMatch (x2, y2) ->
          RangeMatch (QueryConst.max x1 x2, QueryConst.min y1 y2)
    in

    let gather_entries field entries entry_node : EntrySet.t =
      let entry_id = QueryBdd.getuid entry_node in
      let rec _visit m u : EntrySet.t =
        match u with
        | N {var=p; low=l; high=h} when (AtomicPredicate.field p) = field ->
            EntrySet.union (_visit m l) (_visit (refine_match m p) h)
        | N {uid=i} | L {leaf_uid=i} ->
            EntrySet.singleton (Transition (entry_id, m, i))
      in
      EntrySet.union entries (_visit Wildcard entry_node)
    in

    let table_for_field qf =
      let folder entry_nodes parent current =
        let open QueryBdd in
        match (parent, current) with
        | (None, N {uid=u; var=p}) when (AtomicPredicate.field p)=qf->
            NodeSet.add entry_nodes current
        | Some (N {var=pred1}), N {uid=u; var=pred2} ->
            let field1, field2 =
              AtomicPredicate.field pred1, AtomicPredicate.field pred2 in
            if field1 <> field2 && field2 = qf then
              NodeSet.add entry_nodes current
            else
              entry_nodes
        | _ -> entry_nodes
      in
      let entry_nodes = QueryBdd.fold_df folder NodeSet.empty bdd in

      let entries =
        NodeSet.fold
          entry_nodes
          ~init:EntrySet.empty
          ~f:(gather_entries qf) in

      let entries =
        sort_entries (EntrySet.elements entries) in

      {
        field = Some qf;
        entries = entries;
        is_terminal = false;
      }
    in
    {
      tables = (List.map fields ~f:table_for_field) @ [actions_table]
    }

end
