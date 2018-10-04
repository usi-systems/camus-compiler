
%{
  open Query_Ast
%}

%token <Query_Ast.info * string> STRING_LIT
%token <Query_Ast.info * string> IDENT
%token <Query_Ast.info * int> NUMBER
%token <Query_Ast.info * Int64.t> IPADDR
%token <Query_Ast.info> AND
%token <Query_Ast.info> OR
%token <Query_Ast.info> LT
%token <Query_Ast.info> GT
%token <Query_Ast.info> EQ
%token EOF
%token DOT
%token COLON
%token COMMA
%token SEMICOLON
%token LPAREN RPAREN

%type <Query_Ast.rule list> rule_list
%type <Query_Ast.rule> rule
%type <Query_Ast.expr> query


%start rule_list

%%

/* ----- Basic Grammar ----- */

rule_list:
  | EOF { [] }
  | rule SEMICOLON rule_list { $3 @ [$1] }

rule:
  | query COLON action_list { ($1, $3) }

action_list:
   | callExpr { [$1] }
   | callExpr COMMA action_list { $1 :: $3 }

query:
  | logicOrExpr { $1 }

logicOrExpr:
  | logicAndExpr OR logicOrExpr { Or($1,$3) }
  | logicAndExpr { $1 }

logicAndExpr:
  | relExpr AND logicAndExpr { And($1,$3) }
  | relExpr { $1 }

relExpr:
  | lhsExpr LT constExpr { Lt($1,$3) }
  | lhsExpr GT constExpr { Gt($1,$3) }
  | lhsExpr EQ constExpr { Eq($1,$3) }

lhsExpr:
  | fieldExpr { $1 }
  | callExpr  { $1 }


callExpr:
  | IDENT LPAREN RPAREN { let _,id = $1 in Call(id, []) }
  | IDENT LPAREN callArgs RPAREN { let _,id = $1 in Call(id, List.rev $3) }

callArgs:
  | callArg { [$1] }
  | callArgs COMMA callArg { $3 :: $1 }

callArg:
  |  fieldExpr { $1 }
  |  constExpr { $1 }


fieldExpr:
  | IDENT DOT IDENT { let _,id1 = $1 and _,id2 = $3 in Field(id1, id2) }
  | IDENT { let _,id = $1 in Field("default", id) }

constExpr:
  | STRING_LIT { let _,id = $1 in StringLit(id) }
  | NUMBER { let _,id = $1 in NumberLit(id) }
  | IPADDR { let _,id = $1 in IpAddr(Int64.to_int id) }

