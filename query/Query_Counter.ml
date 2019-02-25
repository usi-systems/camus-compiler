open Core
open Bdd
open Query_Types
open Query_Spec

module RuleID = struct
  include Int
  let format_t t =
    string_of_int t
end

module CounterTableBdd = Bdd(AtomicPredicate)(RuleID)

let conjs_from_form qf =
  let open QueryFormula in
  let open CounterTableBdd.Conj in
  let rec conj_from_form f : CounterTableBdd.Conj.t =
    match f with
    | And (Atom x, y) -> (T x)::(conj_from_form y)
    | And (Not (Atom x), y) -> (F x)::(conj_from_form y)
    | Atom x -> [T x]
    | Not (Atom x) -> [F x]
    | _ -> raise (Failure "Not in cannonical form")
  in
  List.map
    ~f:conj_from_form
    (QueryFormula.to_dnf_list qf)


module IntMap = Map.Make(Int)

module CounterTable = struct
  type field_match =
    | EqMatch of int
    | LtMatch of int
    | GtMatch of int
    | RangeMatch of int * int
    | Wildcard
    [@@deriving compare, sexp]

  type entry_id = int
    [@@deriving compare, sexp]
  type entry = field_match * CounterTableBdd.LabelSet.t * entry_id
    [@@deriving compare, sexp]

  module EntrySet = Set.Make(struct type t = entry [@@deriving compare, sexp] end)

  type t = entry list
    [@@deriving compare, sexp]

  let make_match (pred:AtomicPredicate.t) : field_match =
    let open AtomicPredicate in
    match pred with
    | Lt (_, x) -> LtMatch (QueryConst.to_int x)
    | Gt (_, x) -> GtMatch (QueryConst.to_int x)
    | Eq (_, x) -> EqMatch (QueryConst.to_int x)
    | Lpm (_, a, b) -> raise (Failure "Doesn't support LPM yet")
  
  (* This assumes that m and the new pred are overlapping *)
  let refine_match (m:field_match) (pred:AtomicPredicate.t) : field_match =
    let m2 = make_match pred in
    let mk_range a b =
        RangeMatch (a + 1, b - 1)
    in
    match m, m2 with
    | Wildcard, _ | _, Wildcard -> m2
    | EqMatch x, _ | _, EqMatch x -> EqMatch x
    | GtMatch x1, GtMatch x2 -> GtMatch (Int.max x1 x2)
    | LtMatch y1, LtMatch y2 -> LtMatch (Int.min y1 y2)
    | LtMatch x, GtMatch y -> mk_range y x
    | GtMatch x, LtMatch y -> mk_range x y
    | _ -> raise (Failure "Something is wrong if a RangeMatch is being further refined")
  
  let tidy_range_match m =
    match m with
    | RangeMatch (a, b) when a = b -> EqMatch a
    | _ -> m
  
  let gather_entries entry_node : t =
    let open CounterTableBdd in
    let rec _visit m u : EntrySet.t =
      match u with
      | N {var=p; low=l; high=h} ->
          EntrySet.union (_visit m l) (_visit (refine_match m p) h)
      | L {labels=lbls} when CounterTableBdd.LabelSet.length lbls = 0 -> EntrySet.empty
      | L {leaf_uid=i; labels=lbls} ->
          EntrySet.singleton ((tidy_range_match m), lbls, 0)
    in
    EntrySet.to_list
      (_visit Wildcard entry_node)

  let format_match (m:field_match) : string =
    match m with
    | EqMatch x -> Printf.sprintf "=%d" x
    | LtMatch x -> Printf.sprintf "<%d" x
    | GtMatch x -> Printf.sprintf ">%d" x
    | Wildcard -> "*"
    | RangeMatch (x,y) -> Printf.sprintf "%d->%d" x y

  let rec format_t (tbl:t) : string =
    match tbl with
    | [] -> ""
    | (m, lbls, eid)::tbl2 -> Printf.sprintf "%s -> [%s] %d\n%s"
          (format_match m) (CounterTableBdd.fmt_lbls lbls) eid (format_t tbl2)

  let size (tbl:t) : int =
    List.length tbl

  let give_id (entries: t) : t =
    List.mapi entries ~f:(fun i (fm, lbls, _) -> (fm, lbls, i+1))

  let from_bdd (bdd:CounterTableBdd.t) : t =
    give_id (gather_entries bdd.root)

end

module AggTable = struct
  type entry = entry_match * action_set
    [@@deriving compare, sexp]
  and entry_match = entry_id FieldMap.t
    [@@deriving compare, sexp]
  and action_set = Int.Set.t
    [@@deriving compare, sexp]
  and entry_id = int
    [@@deriving compare, sexp]

  type t = entry list
    [@@deriving compare, sexp]

  module EntrySet = Set.Make(struct type t = entry [@@deriving compare, sexp] end)
  module MatchSetMap = Map.Make(struct type t = entry_id FieldMap.t [@@deriving compare, sexp] end)

  let rec int_list_to_str l =
    match l with
    | [] -> ""
    | i::t -> Printf.sprintf "%d, %s" i (int_list_to_str t)

  let format_match (m:entry_match) fields: string =
    List.fold_left ~init:"" ~f:(fun s f -> s ^ (Printf.sprintf "%d\t" (FieldMap.find_exn m f))) fields

  let format_action_set (a:action_set) : string =
    int_list_to_str (Int.Set.to_list a)

  let format_t (tbl:t) : string =
    let fields = match tbl with (m,_)::_ -> FieldMap.keys m | [] -> [] in
    let tbl_stats = Printf.sprintf "%d agg table entries\n" (List.length tbl) in
    let header = List.fold_left ~init:"" ~f:(fun s (h, f) ->
      s ^ (Printf.sprintf "%s.%s\t" h f)) fields in
    let rec _rec (tbl:t) : string =
      match tbl with
      | [] -> ""
      | (m, a)::tbl2 ->
          (Printf.sprintf "%s -> %s\n" (format_match m fields) (format_action_set a)) ^ (_rec tbl2)
    in
    tbl_stats ^ header ^ "\n" ^ (_rec tbl)

  let format_field_tables (field_tables:CounterTable.t FieldMap.t) : string =
    FieldMap.fold field_tables ~init:"" ~f:(fun ~key:(h,f) ~data:tbl s ->
      s ^ (Printf.sprintf "----- %s.%s (%d entries) -----\n%s" h f
              (CounterTable.size tbl) (CounterTable.format_t tbl)))

  let make (rule_map:QueryAction.t list IntMap.t) (field_tables:CounterTable.t FieldMap.t) : t =
   (*
    Printf.printf "\n%s\n" (format_field_tables field_tables);
    *)
    let fields : (string * string) list = FieldMap.keys field_tables in
    let all_entry_ids : entry_id list FieldMap.t =
      FieldMap.map field_tables ~f:(fun tbl -> List.map tbl ~f:(fun (_, _, entry_id) -> entry_id))
    in
    let entries_for_rule rule_id : entry_id list FieldMap.t =
      let entries = FieldMap.map field_tables ~f:(fun tbl -> List.filter_map tbl
          ~f:(fun (_, lbls, entry_id) ->
            if CounterTableBdd.LabelSet.mem lbls rule_id then Some entry_id else None))
      in
      (* if a rule doesn't check a field, it implicitly accepts all entry_ids from that field, plus 0 *)
      let add_wildcard entries field =
        if List.length (FieldMap.find_exn entries field) > 0
        then entries
        else FieldMap.add entries ~key:field ~data:(0::(FieldMap.find_exn all_entry_ids field))
      in
      List.fold_left ~init:entries ~f:add_wildcard fields
    in
    let add_match_set (match_map:Int.Set.t MatchSetMap.t) (ms: entry_id FieldMap.t) rule_id : Int.Set.t MatchSetMap.t =
      MatchSetMap.update match_map ms ~f:(function
        | None -> Int.Set.singleton rule_id
        | Some rule_set -> Int.Set.add rule_set rule_id)
    in
    let merge_rule ~key:rule_id ~data:_ entries =
      let rule_entries = entries_for_rule rule_id in
      (*
      Printf.printf "Rule_id: %d\n" rule_id;
      FieldMap.iteri rule_entries ~f:(fun ~key:(h, f) ~data:el ->
        Printf.printf "\t%s.%s: [%s]\n" h f (int_list_to_str el));
      *)
      let rec add_rules entries ms remaining_fields =
        match remaining_fields with
        | [] -> add_match_set entries ms rule_id
        | f::remaining_fields2 ->
            let entry_ids = FieldMap.find_exn rule_entries f in
            List.fold_left ~init:entries ~f:(fun entries eid ->
                let ms2 = FieldMap.add ms ~key:f ~data:eid in
                add_rules entries ms2 remaining_fields2)
              entry_ids
      in
      add_rules entries FieldMap.empty fields
    in
    let entries = IntMap.fold rule_map ~init:MatchSetMap.empty ~f:merge_rule in
    MatchSetMap.fold entries ~init:[] ~f:(fun ~key:m ~data:rule_ids l ->
      (m, rule_ids)::l)
end

let tables_from_rules (qs:QuerySpec.t) rules =
  let init_field_bdds: CounterTableBdd.t FieldMap.t = FieldMap.empty in
  let add_pred rule_id ~key:field ~data:preds field_bdds =
    FieldMap.update field_bdds field ~f:(fun x ->
      let bdd = (match x with None -> CounterTableBdd.init () | Some bdd -> bdd) in
      (*
      Printf.printf "rule%d: %s\n" rule_id (CounterTableBdd.Conj.format_t preds);
      *)
      let lbls = CounterTableBdd.LabelSet.of_list [rule_id] in
      bdd.root <- CounterTableBdd.merge_nodes bdd bdd.root (CounterTableBdd.conj_to_bdd bdd preds lbls);
      bdd)
  in
  let add_conj rule_id field_bdds conj =
    let find_field_preds field_preds pred =
      let open CounterTableBdd.Conj in
      match pred with
      | T q ->
        let field = match AtomicPredicate.field q with HeaderField (h, f, _, _) -> (h, f) in
        FieldMap.update field_preds field ~f:(function
          | None -> [pred]
          | Some preds -> pred::preds)
      | F _ -> raise (Failure "Counting does not support negation")
    in
    let field_preds : CounterTableBdd.Conj.t FieldMap.t =
      List.fold_left ~init:FieldMap.empty ~f:find_field_preds conj in
    FieldMap.fold field_preds ~init:field_bdds ~f:(add_pred rule_id)
  in
  let add_rule i (rule_map, field_bdds) (qf, act) =
    let rule_id = i + 1 in
    let conjs = conjs_from_form qf in
    (IntMap.add rule_map ~key:rule_id ~data:act,
    List.fold_left ~init:field_bdds ~f:(add_conj rule_id) conjs)
  in
  let (rule_map : QueryAction.t list IntMap.t), (field_bdds : CounterTableBdd.t FieldMap.t) =
    List.foldi ~init:(IntMap.empty, init_field_bdds) ~f:add_rule rules
  in
  (*
  FieldMap.iteri field_bdds ~f:(fun ~key:(h, f) ~data:bdd ->
    Printf.printf "bdd for %s.%s:\ndigraph %s\n" h f (CounterTableBdd.format_t bdd));
    *)
  let field_tables : CounterTable.t FieldMap.t =
    FieldMap.map field_bdds ~f:CounterTable.from_bdd
  in
  let agg_table : AggTable.t = AggTable.make rule_map field_tables in
  FieldMap.iteri field_tables ~f:(fun ~key:(h, f) ~data:tbl ->
      Printf.printf "table %s.%s: %d entries\n" h f (CounterTable.size tbl));
  Printf.printf "Agg table: %d entries\n" (List.length agg_table);
  (*
  Printf.printf "\n%s\n" (AggTable.format_t agg_table);
  *)
  agg_table
  

