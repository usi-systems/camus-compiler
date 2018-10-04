open Core
open Query_Types
open Query_Spec
open Query_Bdd

module QueryTable : sig
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

  module EntrySet : module type of Set.Make(struct type t = entry [@@deriving compare, sexp] end)

  type t = {
    field: QueryField.t option;
    entries: entry list;
    is_terminal: bool;
    }
    [@@deriving compare, sexp]

  val format_t : t -> string
  val format_match : field_match -> string
  val table_name : t -> string
end

module QueryTablePipeline : sig
  type t = {
    tables: QueryTable.t list;
    }
    [@@deriving compare, sexp]

  val create : QuerySpec.t -> QueryBdd.t -> t
  val to_dot : QueryBdd.t -> t -> string
end
