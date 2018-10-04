open Core
open Bdd
open Query_Spec
open Query_Types

module QueryBdd : module type of Bdd(AtomicPredicate)(QueryAction)

val bdd_from_rules : ?slices:int -> QuerySpec.t -> QueryRule.t list -> QueryBdd.t
