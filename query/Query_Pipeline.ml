open Core
open Query_Types
open Query_Spec
open Bignum

let mk_query_exact_tbl_name h f =
      "query_" ^ h ^ "_" ^ f ^ "_exact"

let mk_query_range_tbl_name h f =
      "query_" ^ h ^ "_" ^ f ^ "_range"

let mk_query_miss_tbl_name h f =
      "query_" ^ h ^ "_" ^ f ^ "_miss"

let mk_query_counter_tbl_name h f =
      "query_" ^ h ^ "_" ^ f ^ "_counter"

let mk_query_counter_window_tbl_name h f =
      "query_window_" ^ f

let mk_counter_action_name f =
      "query_" ^ f ^ "_update_counter"

let mk_window_meta_name f = f ^ "_reset_window"


let gen_query_control (qs:QuerySpec.t) : string =
  let tbls =
    List.fold_right
    qs.fields
    ~init:""
    ~f:(fun qf t ->
        let open QuerySpec in
        let h, f, m = match qf with HeaderField(h, f, m, _) -> h, f, m in
        t ^ (if m = ExactAndRangeMatch || m = ExactMatch then
            "  table " ^ (mk_query_exact_tbl_name h f) ^ " {
    key = { meta.query.state: exact; hdr." ^ h ^ "." ^ f ^ ": exact; }
    actions = { set_next_state; NoAction; }
    size = 1024;
    default_action = NoAction();
  }\n"
            else "")
        ^ (if m = ExactAndRangeMatch || m = RangeMatch then
             "  table " ^ (mk_query_range_tbl_name h f) ^ " {
    key = { meta.query.state: exact; hdr." ^ h ^ "." ^ f ^ ": range; }
    actions = { set_next_state; NoAction; }
    size = 1024;
    default_action = NoAction();
  }\n"
            else "")
        ^ "  table " ^ (mk_query_miss_tbl_name h f) ^ " {
    key = { meta.query.state: exact; }
    actions = { set_next_state; NoAction; }
    size = 1024;
    default_action = NoAction();
  }\n\n")
  in
  (* XXX the control flow should be in the order of the field annotations *)
  let control =
    (List.fold_right
    qs.fields
    ~init:"  apply {\n    meta.query.state = 0;\n"
    ~f:(fun qf t ->
        let open QuerySpec in
        let h, f, m = match qf with HeaderField(h, f, m, _) -> h, f, m in
        t ^ (if m = RangeMatch || m = ExactAndRangeMatch then
          "    if (!" ^ (mk_query_range_tbl_name h f) ^ ".apply().hit)\n"
          else "")
        ^ (if m = ExactMatch || m = ExactAndRangeMatch then
          "    if (!" ^ (mk_query_exact_tbl_name h f) ^ ".apply().hit)\n"
          else "")
        ^ "      "  ^ (mk_query_miss_tbl_name h f) ^ ".apply();\n\n"))
    ^ "    query_actions.apply();\n  }"
  in
"struct query_meta_t {
  bit<16> state;
}
struct camus_meta {
  query_meta_t query;
}
control Camus(inout headers hdr,
              inout standard_metadata_t standard_metadata) {
  camus_meta meta;
  action set_next_state(bit<16> next_state) {
    meta.query.state = next_state;
  }
  
  action set_mgid(bit<16> mgid) {
    standard_metadata.mcast_grp = mgid;
  }
  
  action set_egress_port(bit<9> port) {
    standard_metadata.egress_spec = port;
  }
  
  action query_drop() {
    mark_to_drop();
  }

  table query_actions {
    key = { meta.query.state: exact; }
    actions = { query_drop; set_egress_port; set_mgid; }
    size = 1024;
    default_action = query_drop();
  }\n\n" ^ tbls ^ control ^ "\n}"

