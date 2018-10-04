open Core

module type Atom = sig
  type t
    [@@deriving compare, sexp]
  val compare : t -> t -> int
  val format_t : t -> string
  val disjoint : t -> t -> bool
  val subset : t -> t -> bool

end

module Formula (A: Atom) = struct

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

  let cmp_atom = A.compare
  let disjoint_atoms = A.disjoint
  let subset_atoms = A.subset
  let format_atom = A.format_t

  let rec format_t t =
    match t with
    | True -> "T"
    | False -> "F"
    | Atom(x) -> A.format_t x
    | Not(x) -> Printf.sprintf "¬(%s)" (format_t x)
    | And(x, y) -> Format.sprintf "(%s ∧ %s)" (format_t x) (format_t y)
    | Or(x, y) -> Format.sprintf "(%s ∨ %s)" (format_t x) (format_t y)

  let rec eval t a v =
    match t with
    | (True|False) as x -> x
    | Atom(x) when a=x -> if v then True else False
    | Atom(x) when v && disjoint_atoms x a -> False
    | Atom(x) when (not v) && subset_atoms x a -> False
    | Atom(x) when v && subset_atoms a x -> True
    | (Atom _) as x -> x
    | Not(x) ->
        begin
          match eval x a v with
          | True -> False
          | False -> True
          | p -> Not(p)
        end
    | And(x, y) ->
        begin
          match (eval x a v, eval y a v) with
          | (True, True) -> True
          | (False, _) | (_, False) -> False
          | (p, True) | (True, p) -> p
          | (p, q) -> And(p, q)
        end
    | Or(x, y) ->
        begin
          match (eval x a v, eval y a v) with
          | (True, _) | (_, True) -> True
          | (p, False) | (False, p) -> p
          | (p, q) -> Or(p, q)
        end

  let rec to_nnf t =
    match t with
    | (True|False) as x -> x
    | Not((True|False) as x) -> if x=True then False else True
    | Atom(_) as p -> p
    | Not(Atom _) as p -> p
    | Not(Not p) -> to_nnf p
    | Not(And((Atom(_) as p), (Atom(_) as q))) -> Or(Not(p), Not(q))
    | Not(Or((Atom(_) as p), (Atom(_) as q))) -> And(Not(p), Not(q))
    | Not(And(p, q)) -> Or(to_nnf (Not(p)), to_nnf (Not(q)))
    | Not(Or(p, q)) -> And(to_nnf (Not(p)), to_nnf (Not(q)))
    | And(p, q) -> And(to_nnf p, to_nnf q)
    | Or(p, q) -> Or(to_nnf p, to_nnf q)

  let rec conj_fold f acc conj = match conj with
    | And(((And(_,_) as c1)), ((And(_,_) as c2))) ->
          conj_fold f (conj_fold f acc c1) c2
    | And(((And(_,_) as c)), a)
    | And(a, ((And(_,_) as c))) ->
          conj_fold f (f acc a) c
    | And(a, b) ->
          f (f acc a) b
    | a -> f acc a


  let conj_contains conj p =
    conj_fold (fun acc q -> if acc then acc else p = q) false conj

  let cmp_conj_atom a b = match (a, b) with
     | (Not(Atom(x)), Atom(y)) when x = y -> 1
     | (Atom(x), Not(Atom(y))) when x = y -> -1
     | (Not(Atom(x)), Not(Atom(y)))
     | (Atom(x), Not(Atom(y)))
     | (Not(Atom(x)), Atom(y))
     | (Atom(x), Atom(y)) -> cmp_atom x y
     | _ ->
           raise (Failure "Conj should only contain Atom or Not(Atom)")

  let rec fold_atoms f acc t = match t with
     | (True|False) -> acc
     | Atom(a) -> f acc a
     | Not(x) -> fold_atoms f acc x
     | Or(x, y) | And(x, y) -> fold_atoms f (fold_atoms f acc y) x


  let atoms t =
    (* TODO return unique atoms in formula *)
    List.sort A.compare
       (fold_atoms (fun acc a -> a::acc) [] t)

  let conj_dedup conj =
     let f acc p = match acc with
        | None -> Some p
        | Some c when conj_contains c p -> Some c
        | Some c -> Some (And(p, c))
     in
     match conj_fold f None conj with
     | Some c2 -> c2
     | None -> raise (Failure "This should not happen")


  let is_conj_satisfiable conj =
     let is_opposite p q = match (p, q) with
        | (Not(Atom(a)), Atom(b)) when a = b -> true
        | (Atom(a), Not(Atom(b))) when a = b -> true
        | _ -> false
     in
     let contains_opposite acc p =
        if acc then acc else
           conj_fold (fun acc q -> if acc then acc else is_opposite p q) false conj
     in
     not (conj_fold contains_opposite false conj)

  let conj_to_list c =
     List.sort cmp_conj_atom
        (conj_fold
           (fun acc x -> x::acc)
           [] c)



  let conj_is_subset conj_sup conj_sub =
     conj_fold (fun acc p -> if acc then (conj_contains conj_sup p) else acc) true conj_sub

  let conj_eq conj1 conj2 =
     (conj_is_subset conj1 conj2) && (conj_is_subset conj2 conj1)


  let rec disj_fold f acc disj = match disj with
     | Or(((Or(_,_) as d1)), ((Or(_,_) as d2))) ->
           disj_fold f (disj_fold f acc d1) d2
     | Or(((Or(_,_) as d)), a)
     | Or(a, ((Or(_,_) as d))) ->
           disj_fold f (f acc a) d
     | Or(a, b) ->
           f (f acc a) b
     | a -> f acc a


  let disj_map f disj =
     let mapper acc x = match acc with
        | None -> Some (f x)
        | Some d -> Some (Or(d, f x))
     in
     match disj_fold mapper None disj with
     | Some d2 -> d2
     | None -> raise (Failure "This should not happen")


  let disj_filter f disj =
     let filterer acc x = match (acc, f x) with
        | (None, true) -> Some x
        | (None, false) -> None
        | (Some y, true) -> Some (Or(y, x))
        | (Some y, false) -> Some y
     in
     match disj_fold filterer None disj with
     | Some d2 -> d2
     | None -> raise (Failure "This should not happen")


  let disj_contains disj conj =
     let f acc conj2 = match acc with
        | true -> true
        | false ->
              conj_eq conj conj2
     in
     disj_fold f false disj


  let disj_dedup disj =
     let f acc conj = match acc with
        | None -> Some conj
        | Some d when disj_contains d conj -> Some d
        | Some d -> Some (Or(d, conj))
     in
     match disj_fold f None disj with
     | Some d2 -> d2
     | None -> raise (Failure "This should not happen")

  let rec list_to_conj conj_atom_list = match conj_atom_list with
        | a::[] -> a
        | a::l2 -> And(a, list_to_conj l2)
        | [] -> raise (Failure "Empty conjunction")

  let disj_to_list d =
     disj_fold (fun acc c -> (match conj_to_list c with
        | [] -> acc
        | l -> l::acc))
     [] d

  let dnf_canonicalize disj =
     let rec cmp_atom_list a b = match (a, b) with
        | (x::a2, y::b2) -> let c = cmp_conj_atom x y in
              if c = 0 then cmp_atom_list a2 b2 else c
        | ([], []) -> 0 | ([], _) -> -1 | (_, []) -> 1
     in
     let rec list_to_disj conj_list = match conj_list with
        | c::[] -> c
        | c::l2 -> Or(c, list_to_disj l2)
        | [] -> raise (Failure "Empty disjunction")
     in
     list_to_disj
        (List.map
           (List.sort cmp_atom_list
              (disj_to_list disj))
           ~f:list_to_conj
           )


    let to_dnf t =
      let rec dist_left l r = match r with
         | Or(p, q) -> Or(dist_left l p, dist_left l q)
         | _ -> And(l, r)
      in
      let rec dist_right l r = match l with
         | Or(p, q) -> Or(dist_right p r, dist_right q r)
         | _ -> dist_left l r
      in
      let rec dist = function
         | Or(p, q) -> Or(dist p, dist q)
         | And(p, q) -> dist_right (dist p) (dist q)
         | t -> t
      in
      let unreduced_dnf = dist (to_nnf t) in
      dnf_canonicalize (                              (* canonicalize the tree structure *)
         disj_dedup (                                 (* remove duplicate conjunctions *)
            disj_filter is_conj_satisfiable           (* remove unsatisfiable conjunctions *)
               (disj_map conj_dedup unreduced_dnf)))  (* remove duplicate preds *)

  (* Return a predicate in `conj` that's an ancestor of `pred`, if any. *)
  let rec preceding_atom atom conj = match conj with
     | And(a, b) -> (match preceding_atom atom a with
           | None -> preceding_atom atom b
           | (Some _) as x -> x)
     | Atom x when (cmp_atom x atom) < 0 -> Some (x, true)
     | Not (Atom x) when (cmp_atom x atom) < 0 -> Some (x, false)
     | _ -> None

  let rec first_atom conj = match conj with
     | And(a, b) -> first_atom a
     | Atom x -> (x, true)
     | (Not (Atom x)) -> (x, false)
     | _ -> raise (Failure "Bad format for conj")

  let to_dnf_list t =
    let rec _to_list t =
      match t with
      | Or(x, y) -> x::(_to_list y)
      (* TODO: shouldn't we check: Not _ -> [t] *)
      | And _ -> [t]
      | Atom _ -> [t]
      | _ -> raise (Failure ("Formula should be in canonical DNF: " ^ (format_t t))) in
    let dnf = to_dnf t in
    _to_list dnf


end


(*
module IntAtom = struct
  type t = int
    [@@deriving compare, sexp]
  let compare a b = compare a b
  let format_t t = string_of_int t
  let disjoint t1 t2 = false
  let subset t1 t2 = false
end

module IntFormula = Formula(IntAtom)

let foo () =
  let open IntFormula in
  let a, b, c, d, e = Atom(1), Atom(2), Atom(3), Atom(4), Atom(5) in
  let f = And(Or(a, b), Or(c, d)) in
  let f = IntFormula.to_dnf f in
  let f2 = IntFormula.eval f 1 true in
  let f3 = IntFormula.eval f2 3 false in
  print_endline (IntFormula.format_t f);
  print_endline (IntFormula.format_t f2);
  print_endline (IntFormula.format_t f3);
  ()
*)
