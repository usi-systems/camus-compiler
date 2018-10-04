open Core

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
  
module Conjunction (V: BddVar) : sig
  type true_or_false =
    | T of V.t
    | F of V.t
  type t =
    true_or_false list
  val format_t : t -> string
  val implies : t -> t -> bool
  val eval : V.assignments -> t -> bool
  val sort : t -> t
end
  

module Bdd (V: BddVar) (L: BddLabel) : sig

  module Label : module type of L

  module VarSet : module type of Set.Make(V)
  module LabelSet : module type of Set.Make(L)

  module Conj : module type of Conjunction(V)

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

  val getuid : node -> uid
  val getlabels : node -> LabelSet.t

  val node_equal : node -> node -> bool

  module NodeH : sig
    type t = node
    val equal : t -> t -> bool
    val hash : t -> int
  end
  module NodeWeakHS : module type of Caml.Weak.Make(NodeH)

  module N : sig
    type t = node
      [@@deriving compare, sexp]
  end
  module NodeSet : module type of Set.Make(N)

  type t = {
    table: NodeWeakHS.t;
    mutable next_uid: int;
    mutable root: node;
    empty_leaf: node;
  }


  val merge_nodes : t -> node -> node -> node
  val dump_dot : t -> string -> unit
  val conj_to_bdd : t -> Conj.t -> LabelSet.t -> node
  
  val eval_bdd : t -> V.assignments -> node -> LabelSet.t
  val find_paths : ?path:Conj.t -> node -> ((Conj.t * LabelSet.t) list)

  val format_t : ?graph_name:string -> t -> string
  
  val fold_df : ('a -> node option -> node -> 'a) -> 'a -> t -> 'a
  val fold_tree : ('b -> node -> (bool * 'b)) -> 'b -> node -> 'b list
  val vars : t -> VarSet.t
  val leaves : t -> NodeSet.t

  val getnode : t -> uid -> node

  val init : unit -> t
end
