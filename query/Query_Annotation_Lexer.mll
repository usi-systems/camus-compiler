{
open Core
open Query_Annotation_Parser

type codeinfo = string * (int * int) * (int * int)

exception LexerError of codeinfo

let info_cell = ref None

let get_info () = !info_cell

let filename () = match get_info () with
  | None -> failwith "Filtered_Annotation_Lexer: no location information."
  | Some (fn,_,_) -> fn
let line () = match get_info () with
  | None -> failwith "Filtered_Annotation_Lexer: no location information."
  | Some (_,l,_) -> l
let column () = match get_info () with
  | None -> failwith "Filtered_Annotation_Lexer: no location information."
  | Some (_,_,c) -> c

let set_filename fn = match get_info () with
  | None -> failwith "Filtered_Annotation_Lexer: no location information."
  | Some (_,l,c) -> info_cell := Some (fn,l,c)
let set_line l = match get_info () with
  | None -> failwith "Filtered_Annotation_Lexer: no location information."
  | Some (fn,_,c) -> info_cell := Some (fn,l,c)
let set_column c = match get_info () with
  | None -> failwith "Filtered_Annotation_Lexer: no location information."
  | Some (fn,l,_) -> info_cell := Some (fn,l,c)

let setup fn = info_cell := Some (fn,1,0)

let newline lexbuf : unit =
  set_column (Lexing.lexeme_start lexbuf);
  set_line (line () + 1)

let newline lexbuf : unit =
  set_column (Lexing.lexeme_start lexbuf);
  set_line (line () + 1)

let info lexbuf : codeinfo =
  let c1 = Lexing.lexeme_start lexbuf in
  let c2 = Lexing.lexeme_end lexbuf in
  let l = line () in
  let c = column () in
  let fn = filename () in
  (fn, (l, c1 - c - 1),(l, c2 - c - 1))

}

let whitespace = [' ' '\t']+
let newline = "\n"
let decimal = ['0'-'9']+

let ident = ['a'-'z' 'A'-'Z' '_' '$']['a'-'z' 'A'-'Z' '0'-'9' '_' '!' '$']*

rule token = parse
  | whitespace { token lexbuf }
  | newline { newline lexbuf; token lexbuf }
  | "query_field" { QUERY_FIELD }
  | "query_field_exact" { QUERY_FIELD_EXACT }
  | "query_field_range" { QUERY_FIELD_RANGE }
  | "query_field_counter" { QUERY_FIELD_COUNTER }
  | "query_control" { QUERY_CONTROL }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "." { DOT }
  | "," { COMMA }
  | ident as x { ID(x) }
  | decimal as integ {
        NUMBER(integ)
      }
  | _  { raise (LexerError(info lexbuf)) }
