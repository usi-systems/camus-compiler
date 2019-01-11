open Core
open Bdd
open Query_Types
open Query_Spec

module StringMap = Map.Make(String)

module QueryBdd = Bdd(AtomicPredicate)(QueryAction)

let conjs_from_form qf =
  let open QueryFormula in
  let open QueryBdd.Conj in
  let rec conj_from_form f : QueryBdd.Conj.t =
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

let bdd_from_rules ?slices:(slices=40) (qs:QuerySpec.t) rules =
  let bdd = QueryBdd.init () in
  let cnt = ref 0 in
  let start_time = Unix.gettimeofday () in
  let last_time = ref (Unix.gettimeofday ()) in
  let rec split l n =
    if n = 1 then [l]
    else if l = [] then []
    else
      let h, t = List.split_n l ((List.length rules)/slices) in
      h::(split t (n-1))
  in
  let rules_slices = split rules slices in
  let merge_rule m (qf, a) =
      cnt := !cnt + 1;
      if !cnt mod 1000 = 0
      then
        begin
          let time_now = Unix.gettimeofday () in
          Printf.printf "%d\t%f\n" !cnt (time_now -. !last_time);
          Out_channel.flush stdout;
          last_time := time_now;
        end;
      let conjs = conjs_from_form qf in
      let lbls = QueryBdd.LabelSet.of_list a in
      List.fold_left
        ~init:m
        ~f:(fun m conj ->
          QueryBdd.merge_nodes bdd m (QueryBdd.conj_to_bdd bdd conj lbls))
        conjs
  in
  let bdd_for_slice rules_slice =
    List.fold_left ~init:bdd.empty_leaf ~f:merge_rule rules_slice
  in
  let slice_bdds =
    List.map ~f:bdd_for_slice rules_slices
  in
  bdd.root <- List.fold_left ~init:bdd.empty_leaf ~f:(QueryBdd.merge_nodes bdd) slice_bdds;
  Printf.printf "Made BDD in %fs\n" ((Unix.gettimeofday ()) -. start_time);
  Out_channel.flush stdout;
  bdd
