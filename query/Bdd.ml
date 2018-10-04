open Core

(*
 * Based on: https://braibant.github.io/update/2014/06/17/bdd-1.html
 *)

module type BddVar = sig
  type t
    [@@deriving compare, sexp]
  type assignments

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val disjoint : t -> t -> bool
  val subset : t -> t -> bool
  val independent : t -> t -> bool

  val eval : assignments -> t -> bool

  val hash : t -> int
  val format_t : t -> string
end

module type BddLabel = sig
  type t
    [@@deriving compare, sexp]
  val compare : t -> t -> int
  val format_t : t -> string
end

module Conjunction (V: BddVar) = struct
  type true_or_false =
    | T of V.t
    | F of V.t

  type t =
    true_or_false list

  let format_t (conj:t) : string =
    let fmt_tof x = match x with
    | T v -> V.format_t v
    | F v -> Printf.sprintf "~(%s)" (V.format_t v)
    in
    String.concat ~sep:" AND " (List.map ~f:fmt_tof conj)

  (* Does a path (conj1) through the BDD satisfy the conjuction? *)
  (* conj1 --> conj2 *)
  let rec implies (conj1:t) (conj2:t) : bool =
    let impl_true x y = (* y --> x *)
      match x, y with
      | T v2, T v1 | F v1, F v2 -> V.subset v1 v2
      | _ -> false
    in
    let impl_false x y = (* y --> ~x *)
      match x, y with
      | T v1, T v2 -> V.disjoint v1 v2
      | T v1, F v2 | F v2, T v1 -> V.subset v1 v2
      | _ -> false
    in
    match conj2 with
    | x::t ->
        (List.exists conj1 ~f:(impl_true x))              (* the atom x should be true along the path *)
        && (not (List.exists conj1 ~f:(impl_false x)))    (* and never false *)
        && (implies conj1 t)                              (* check the remaning atoms in the conj *)
    | [] -> true

  let sort (conj:t) : t =
    let cmp a b =
      match a, b with
      | T v1, F v2 when V.equal v1 v2 -> 1
      | F v1, T v2 when V.equal v1 v2 -> -1
      | T v1, F v2 | F v1, T v2 | T v1, T v2 | F v1, F v2 ->
          V.compare v1 v2
    in
    List.sort ~cmp:cmp conj

  let rec eval (a: V.assignments) (conj:t) : bool =
    match conj with
    | [] -> true
    | (T v)::t when V.eval a v -> eval a t
    | (F v)::t when not (V.eval a v) -> eval a t
    | _ -> false
end


module Bdd (V: BddVar) (L: BddLabel) = struct

  module Label = L

  module VarSet = Set.Make(V)
  module LabelSet = Set.Make(L)

  module Conj = Conjunction(V)

  type node =
  | L of leaf
  | N of decision
      [@@deriving compare, sexp]
  and leaf = {leaf_uid: uid; labels: LabelSet.t}
      [@@deriving compare, sexp]
  and decision = {uid: uid; var: V.t; low: node; high: node}
      [@@deriving compare, sexp]
  and uid = int
      [@@deriving compare, sexp]

  let getuid = function
    | L l -> l.leaf_uid
    | N n -> n.uid

  let node_equal x y =
    match x, y with
    | N n1, N n2 ->
        V.compare n1.var n2.var = 0
        && getuid n1.low = getuid n2.low
        && getuid n1.high = getuid n2.high
    | L l1, L l2 ->
        LabelSet.equal l1.labels l2.labels
    | N _, L _ | L _, N _ -> false

  module NodeH = struct
    type t = node
    let equal = node_equal
    let hash node =
      match node with
      | N n ->
          (Hashtbl.hash (V.hash n.var, getuid n.low, getuid n.high)) land Int.max_value
      | L l ->
          (Hashtbl.hash l.labels) land Int.max_value
  end
  module NodeWeakHS = Caml.Weak.Make(NodeH)

  module N = struct
    type t = node
      [@@deriving compare, sexp]
  end
  module NodeSet = Set.Make(N)

  type t = {
    table: NodeWeakHS.t;
    mutable next_uid: int;
    mutable root: node;
    empty_leaf: node;
  }

  let mk_node (bdd:t) var low high =
    if getuid low = getuid high
    then low
    else
      begin
        let n1 = N {uid = bdd.next_uid; var; low; high} in
        let n2 = NodeWeakHS.merge bdd.table n1 in
        if phys_equal n1 n2
        then bdd.next_uid <- bdd.next_uid+1;
        n2
      end

  let mk_leaf (bdd:t) lbls =
    let l1 = L {leaf_uid = bdd.next_uid; labels = lbls} in
    let l2 = NodeWeakHS.merge bdd.table l1 in
    if phys_equal l1 l2
      then bdd.next_uid <- bdd.next_uid+1;
    l2

  let rec prune_implicit (bdd:t) (ancestor:V.t) (is_high_branch:bool) (n:node) =
    match n with
    | N {var = var; high = high; low = low; _} ->
        if V.independent ancestor var then (* stop descending the tree *)
          n
        else if is_high_branch && V.disjoint ancestor var then (* implicitly false *)
          prune_implicit bdd ancestor is_high_branch low
        else if is_high_branch && V.subset ancestor var then (* implicitly true *)
          prune_implicit bdd ancestor is_high_branch high
        else if (not is_high_branch) && V.subset var ancestor then (* implicitly false *)
          prune_implicit bdd ancestor is_high_branch low
        else
          mk_node bdd var (prune_implicit bdd ancestor is_high_branch low) (prune_implicit bdd ancestor is_high_branch high)
    | L _ -> n


  let rec merge_nodes (bdd:t) (x:node) (y:node) : node =
    let merge, mk_node, mk_leaf, prune_implicit = merge_nodes bdd, mk_node bdd, mk_leaf bdd, prune_implicit bdd in
    let x,y = match x, y with (* order x and y if they are both internal (decision) nodes *)
    | N {var = var1; _}, N {var = var2; _} ->
        if (V.compare var1 var2) < 0 then (x,y) else (y,x)
    | _ -> (x, y)
    in
    match x, y with
    | L {labels = lbls1}, L {labels = lbls2} ->                 (* both leaves *)
        mk_leaf (LabelSet.union lbls1 lbls2)
    | (L _ as l), (N {var = var; low = low; high = high; _} as n)
    | (N {var = var; low = low; high = high; _} as n), (L _ as l) when node_equal l bdd.empty_leaf ->   (* empty leaf and decision node *)
        n (* this is an optimization; we don't need to push the empty leaf all the way down all branches *)
    | (L _ as l), N {var = var; low = low; high = high; _}
    | N {var = var; low = low; high = high; _}, (L _ as l) ->   (* leaf and decision node *)
        mk_node var (merge low l) (merge high l)
    | N {var = var1; low = low1; high = high1; _},              (* both decision nodes *)
      N {var = var2; low = low2; high = high2; _} when V.equal var1 var2 ->
        mk_node var1 (merge low1 low2) (merge high1 high2)
    | N {var = var1; low = low1; high = high1; _}, (* already sorted; var1 comes before var2 in the BDD ordering *)
      N {var = var2; low = low2; high = high2; _} ->            (* both nodes *)
        begin
          if V.disjoint var1 var2
          then
            mk_node var1 (merge low1 (prune_implicit var1 false y)) (merge (prune_implicit var1 true low2) high1)
          else if V.subset var2 var1 (* var2=true --> var1=true *)
          then
            mk_node var1 (merge low1 (prune_implicit var1 false low2)) (merge high1 (prune_implicit var1 true y))
          else if V.subset var1 var2 (* var1=true --> var2=true *)
          then
            mk_node var1 (merge low1 (prune_implicit var1 false y)) (merge high1 (prune_implicit var1 true high2))
          else
            mk_node var1 (merge low1 (prune_implicit var1 false y)) (merge high1 (prune_implicit var1 true y))
        end

  let fmt_lbls (lbls:LabelSet.t) : string =
    String.concat ~sep:", " (List.map ~f:L.format_t (LabelSet.to_list lbls))

  let rec conj_to_bdd (bdd:t) (formula:Conj.t) (lbls:LabelSet.t) =
    let conj_to_bdd, mk_node, mk_leaf = conj_to_bdd bdd, mk_node bdd, mk_leaf bdd in
    let sorted_formula = Conj.sort formula in
    match sorted_formula with
    | T q::[] -> mk_node q bdd.empty_leaf (mk_leaf lbls)
    | F q::[] -> mk_node q (mk_leaf lbls) bdd.empty_leaf
    | T q::t -> mk_node q bdd.empty_leaf (conj_to_bdd t lbls)
    | F q::t -> mk_node q (conj_to_bdd t lbls) bdd.empty_leaf
    | _ -> raise (Failure "unreachable")

  let rec eval_bdd (bdd:t) (a: V.assignments) (u:node) : LabelSet.t =
    let eval_bdd = eval_bdd bdd in
    match u with
    | N {var = v; low = l; high = h} ->
        if V.eval a v then
          eval_bdd a h
        else
          eval_bdd a l
    | L {labels = lbls} -> lbls

  let rec find_paths ?path:(path=[]) (x:node) : ((Conj.t * LabelSet.t) list)=
    let open Conj in
    match x with
    | N {var=v; low=l; high=h} ->
        (find_paths ~path:((F v)::path) l) @ (find_paths ~path:((T v)::path) h)
    | L {labels=lbls} ->
        [(path, lbls)]

  let fold_df (f:('a -> node option -> node -> 'a)) (init:'a) (bdd:t) : 'a =
    let rec visitor (parent:node option) (u:node) (acc:'a) =
      match u with
      | N {low=l; high=h} ->
        f acc parent u |> visitor (Some u) l |> visitor (Some u) h
      | L _ -> f acc parent u
    in
    visitor None bdd.root init

  let fold_tree (f:('a -> node -> (bool * 'a))) (init:'a) (start:node) : 'a list =
    let rec visitor (acc:'a) (u:node) : 'a list =
      let continue, acc2 = f acc u in
      match continue, u with
      | false, _ -> [acc2]
      | true, L _ -> [acc2]
      | true, N {low=l; high=h} ->
          (visitor acc2 l) @ (visitor acc2 h)
    in
    visitor init start

  let leaves (bdd:t) : NodeSet.t =
    let folder (found:NodeSet.t) (p:node option) (u:node) : NodeSet.t =
      match u with
      | L _ -> NodeSet.add found u
      | _ -> found
    in
    fold_df folder NodeSet.empty bdd

  let vars (bdd:t) : VarSet.t =
    let folder (found:VarSet.t) (parent:node option) (u:node) : VarSet.t =
      match u with
      | N {var=v} -> VarSet.add found v
      | _ -> found
    in
    fold_df folder VarSet.empty bdd

  let format_t ?graph_name:(graph_name="g") (bdd:t) : string =
    let visited = Caml.Hashtbl.create 1337 in
    let rec w (u:node) : string =
      if (Caml.Hashtbl.mem visited u) then ""
      else
        begin
          Caml.Hashtbl.add visited u 0;
          match u with
          | N {uid = i; var = v; low = l; high = h;} ->
              (Printf.sprintf "n%d [label=\"%s\"];\n" i (V.format_t v)) ^
              (Printf.sprintf "n%d -> n%d [style=\"dashed\"];\n" i (getuid l)) ^
              (Printf.sprintf "n%d -> n%d;\n" i (getuid h)) ^
              (w l) ^ (w h)
          | L {leaf_uid = i; labels = lbls } ->
              Printf.sprintf "n%d [label=\"%s\" shape=box style=filled] {rank=sink; n%d};\n" i (fmt_lbls lbls) i
        end
    in
    (Printf.sprintf "%s {\n" graph_name) ^
    (w bdd.root) ^
    "}"

  let getnode (bdd:t) (i:uid) : node =
    let rec visitor (u:node) : node option =
      match u with
      | N {uid=i2; low=l; high=h} ->
          if i = i2 then Some u
          else
            begin
              match visitor l, visitor h with
              | Some n, _ | _, Some n -> Some n
              | _ -> None
            end
      | L {leaf_uid=i2} -> if i = i2 then Some u else None
    in
    let found = visitor bdd.root in
    match found with
    | None -> raise (Failure ("Node not found: " ^ (string_of_int i)))
    | Some n -> n

  let getlabels (n:node) : LabelSet.t =
    match n with
    | L {labels=lbls} -> lbls
    | _ -> raise (Failure "Can only get labels from leaf (terminal) nodes")

  let dump_dot (bdd:t) (fname:string) : unit =
    let oc = Out_channel.create fname in
    Printf.fprintf oc "%s" (format_t bdd);
    Out_channel.close oc


  let init () =
    let empty_leaf = L {leaf_uid = 1; labels = LabelSet.empty} in
    { table = NodeWeakHS.create 1337;
      next_uid = 2;
      root = empty_leaf;
      empty_leaf = empty_leaf;
    }

end
