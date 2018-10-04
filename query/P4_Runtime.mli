open Core
open Query_Table
open Query_Types

module IntSetMap : module type of Map.Make(Int.Set)

module P4Table : sig
  type width = int [@@deriving compare, sexp]

  type value =
    | Number of int
    | String of string
    [@@deriving compare, sexp]

  type action_name = string [@@deriving compare, sexp]

  type action =
    action_name * value list
    [@@deriving compare, sexp]

  type field_match =
    | EqMatch of value * width
    | LtMatch of value * width
    | GtMatch of value * width
    | RangeMatch of value * value * width
    [@@deriving compare, sexp]

  type entry =
    field_match list * action
    [@@deriving compare, sexp]

  type t = {
    name: string;
    fields: QueryField.t list;
    entries: entry list;
    }
    [@@deriving compare, sexp]

  val format_t : t -> string
  val format_value : value -> string
  val from_abstract : QueryTable.t -> int IntSetMap.t -> t list
end

module P4RuntimeConf : sig

  type mcast_group = Int.Set.t
    [@@deriving compare, sexp]

  type t = {
    tables: P4Table.t String.Map.t;
    mcast_groups: mcast_group Int.Map.t;
    }
    [@@deriving compare, sexp]

  val from_abstract : QueryTablePipeline.t -> t
  val format_mcast_groups : t -> string
  val format_commands : t -> string
end
