open Core
open Core_extended.Std
open Bignum
open Async
open Let_syntax
open Query

let parse_rules_string string =
  let open Query_Ast in
  let lexbuf = Lexing.from_string string in
  try
    `Ok (Query_Parser.rule_list Query_Lexer.main lexbuf)
  with
    | Query_Lexer.LexerError(s) ->
      `Error s
    | Query_Parser.Error ->
      let i = Query_Lexer.info lexbuf in
      `Error (Format.sprintf "%s: syntax error" (str_of_info i))
    | _ ->
      let i = Query_Lexer.info lexbuf in
      `Error (Format.sprintf "%s: no such token" (str_of_info i))

let parse_rules_file filename =
  Query_Lexer.setup filename;
  let s = In_channel.read_all filename in
  parse_rules_string s

let go prog_out rt_out dot_out rules_fn slices fn : unit Deferred.t =
  let stem = Filename.chop_extension fn in
   (* Parse *)
   let () = Format.eprintf "[parse: %s]@\n%!" fn in

   (* Parse querry_* annotations *)
   let query_spec = Query_Spec.load_query_spec fn in

   (* Transform using annotations *)
   let control_prog = Query_Pipeline.gen_query_control query_spec in

   (* Optionally, output the generated program *)
   begin
     match prog_out with
     | Some fn ->
         let () = Out_channel.write_all fn
              ~data:control_prog in
         Format.eprintf "[generated: %s]@\n%!" fn
     | None -> ()
   end;

   begin
     match rules_fn with
     | Some fn ->
         let out_base = (match rt_out with Some b -> b | None -> stem) in
         begin
           match parse_rules_file fn with
           | `Ok rules_ast ->
               let open Query_Bdd in
               let open Query_Table in
               let open Query_Spec in
               let open P4_Runtime in
               let rules = Query_Types.QueryRule.from_ast rules_ast in
               let rules = QuerySpec.prioritize_fields query_spec rules in

               let default_slices = max 2 ((List.length rules) / 123) in (* this seems to be the sweet spot *)
               let slices = match slices with
                 | None -> default_slices
                 | Some x -> x in
               let bdd = bdd_from_rules ~slices:slices query_spec rules in

               let tables = QueryTablePipeline.create query_spec bdd in

               begin
                 match dot_out with
                 | Some fn ->
                     let () = Out_channel.write_all fn
                        ~data:(QueryTablePipeline.to_dot bdd tables) in
                     let () = Format.eprintf "[runtime DOT: %s]@\n%!" fn in
                     ()
                 | None -> ()
               end;

               let rtc = P4RuntimeConf.from_abstract tables in

               let () = Out_channel.write_all (out_base ^ "_commands.txt")
                          ~data:(P4RuntimeConf.format_commands rtc) in
               let () = Format.eprintf "[commands: %s_commands.txt]@\n%!" out_base in

               let () = Out_channel.write_all (out_base ^ "_mcast_groups.txt")
                          ~data:(P4RuntimeConf.format_mcast_groups rtc) in
               let () = Format.eprintf "[mcast: %s_mcast_groups.txt]@\n%!" out_base in
               ()
           | `Error msg ->
               Format.eprintf "@[%s@\n%!@]" msg
         end
     | None -> ()
   end;

   return ()

let spec =
  let open Command.Spec in
  empty
  +> flag "-prog-out" (optional string) ~doc:("FILE Path to which the generated P4 program will be stored")
  +> flag "-rt-out" (optional string) ~doc:("FILE Basename of the generated runtime files")
  +> flag "-dot-out" (optional string) ~doc:("FILE Path to save DOT viz of runtime BDD and tables")
  +> flag "-rules" (optional string) ~doc:("FILE Path to input rules file to generate runtime")
  +> flag "-slices" (optional int) ~doc:("N Make N BDDs from the rules, and merge them all")
  +> anon ("filename" %:string)

let command =
  Command.async
    ~summary:"p4queries: generate a pipeline for filtering queries"
    spec
    (fun prog_out rt_out dot_out rules_fn slices fn () -> go prog_out rt_out dot_out rules_fn slices fn)

let () =
  Format.set_margin 160;
  Command.run ~version:"0.1" command
