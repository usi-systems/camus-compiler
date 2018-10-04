{
  open Lexing
  open Query_Parser
  open Query_Ast


  exception LexerError of string

  let info_cell : (string * int * int) option ref = ref None

  let get_info () = !info_cell

  let filename () = match get_info () with
    | None -> failwith "Query_Lexer: no location information."
    | Some (fn,_,_) -> fn
  let line () = match get_info () with
    | None -> failwith "Query_Lexer: no location information."
    | Some (_,l,_) -> l
  let column () = match get_info () with
    | None -> failwith "Query_Lexer: no location information."
    | Some (_,_,c) -> c

  let set_filename fn = match get_info () with
    | None -> failwith "Query_Lexer: no location information."
    | Some (_,l,c) -> info_cell := Some (fn,l,c)
  let set_line l = match get_info () with
    | None -> failwith "Query_Lexer: no location information."
    | Some (fn,_,c) -> info_cell := Some (fn,l,c)
  let set_column c = match get_info () with
    | None -> failwith "Query_Lexer: no location information."
    | Some (fn,l,_) -> info_cell := Some (fn,l,c)

  let setup fn = info_cell := Some (fn,1,0)

  let newline lexbuf : unit =
    set_column (Lexing.lexeme_start lexbuf);
    set_line (line () + 1)

  let info lexbuf : Query_Ast.info =
    let c1 = Lexing.lexeme_start lexbuf in
    let c2 = Lexing.lexeme_end lexbuf in
    let l = line () in
    let c = column () in
    let fn = filename () in
    (fn, (l, c1 - c - 1),(l, c2 - c - 1))

  let error lexbuf msg =
    let i = info lexbuf in
    let t = Lexing.lexeme lexbuf in
    let s = Printf.sprintf "%s: lexing error %s at %s."
      (Query_Ast.str_of_info i) msg t in
    failwith s


  let keywords = Hashtbl.create 53
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok)
      [
	("and", fun i -> AND i)
        ; ("or", fun i -> OR i)
      ]

  let int_of_hex = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
    | 'A' | 'a' -> 10 | 'B' | 'b' -> 11 | 'C' | 'c' -> 12
    | 'D' | 'd' -> 13 | 'E' | 'e' -> 14 | 'F' | 'f' -> 15
    | n -> failwith ("Lexer.int_of_hex: " ^ (String.make 1 n))

  let parse_byte str = Int64.of_string ("0x" ^ str)
  let parse_decbyte str = Int64.of_string str

}

let whitespace = [' ' '\t']+
let newline = "\n"
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let decimal = ['0'-'9']+
let float_ = ['0'-'9']+ '.' ['0'-'9']+
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let int_char = ['0' - '9']
let string_lit = '"' [^'"']* '"'
let hex_char = ['0' - '9' 'A' - 'F' 'a' - 'f']
let decbyte =
     (['0'-'9'] ['0'-'9'] ['0'-'9']) | (['0'-'9'] ['0'-'9']) | ['0'-'9']

rule main =
  parse
  | whitespace         { main lexbuf }
  | "<"                { LT (info lexbuf) }
  | ">"                { GT (info lexbuf) }
  | "="                { EQ (info lexbuf) }
  | ":"                { COLON }
  | "."                { DOT }
  | ","                { COMMA }
  | ";"                { SEMICOLON }
  | "("                { LPAREN }
  | ")"                { RPAREN }
  | "\""               {
                         let s = string_lit "" lexbuf in
                         STRING_LIT(info lexbuf,s)
                       }
  | id as ident {
        try Hashtbl.find keywords ident (info lexbuf)
        with Not_found -> IDENT(info lexbuf, ident)
      }
  | decimal as integ {
        NUMBER(info lexbuf,int_of_string integ)
      }

  | (decbyte as b4) "." (decbyte as b3) "." (decbyte as b2) "." (decbyte as b1)
          { let open Int64 in
            IPADDR(info lexbuf,
              (logor (shift_left (parse_decbyte b4) 24)
                 (logor (shift_left (parse_decbyte b3) 16)
                    (logor (shift_left (parse_decbyte b2) 8)
                       (parse_decbyte b1))))) }

  | newline            { newline lexbuf; main lexbuf  }
  | eof                { EOF }
  | _                  { error lexbuf "unknown token" }

and escape el = parse
    | "\\"          { "\\" }
    | "b"           { "\008" }
    | "n"           { "\010" }
    | "r"           { "\013" }
    | "t"           { "\009" }
    | "0x" (hex_char as h1) (hex_char as h2) {
      String.make 1 (Char.chr (16 * int_of_hex h1 + int_of_hex h2))
    }

    | int_char int_char int_char as c {
      String.make 1 (Char.chr (int_of_string c))
    }
    | _ {
      try List.assoc (lexeme lexbuf) el
      with Not_found -> error lexbuf "in escape sequence"
    }

and string_lit acc = parse
    | "\\"          { let s = escape [("\"","\"");("'","'")] lexbuf in
                      string_lit (acc ^ s) lexbuf }
    | "\""          { acc }
    | newline ([' ' '\t']* "|")?
        { newline lexbuf; string_lit (acc ^ "\n") lexbuf}
    | eof           { error lexbuf "unmatched '\"'" }
    | _             { string_lit (acc ^ lexeme lexbuf) lexbuf }
