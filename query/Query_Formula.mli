open Core

module type Atom = sig
  type t
    [@@deriving compare, sexp]
  val compare : t -> t -> int
  val format_t : t -> string
  val disjoint : t -> t -> bool
  val subset : t -> t -> bool
end

module Formula (A: Atom): sig

  type atom = A.t
  [@@deriving compare, sexp]

  type t =
    | True
    | False
    | Atom of atom
    | Not of t
    | And of t * t
    | Or of t * t
    [@@deriving compare, sexp]

  val cmp_atom : atom -> atom -> int
  val disjoint_atoms : atom -> atom -> bool
  val subset_atoms : atom -> atom -> bool
  val format_t : t -> string
  val format_atom : atom -> string
  val eval : t -> atom -> bool -> t
  val to_nnf : t -> t
  val to_dnf : t -> t
  val to_dnf_list : t -> t list (* list of conjunctions *)
  val atoms : t -> atom list

  val preceding_atom : atom -> t -> (atom * bool) option
  val first_atom : t -> atom * bool

end
