%{
open Core
open Query_Types
%}

%token LPAREN RPAREN DOT COMMA
%token QUERY_FIELD QUERY_FIELD_EXACT QUERY_FIELD_RANGE QUERY_FIELD_COUNTER
%token QUERY_CONTROL
%token<string> ID
%token<string> NUMBER

%start<Query_Types.query_annotation> query_stmt

%%

query_stmt:
  | query_field { $1 }
  | query_field_exact { $1 }
  | query_field_range { $1 }
  | query_field_counter { $1 }
  | query_control { $1 }


query_control:
  | QUERY_CONTROL   { Query_Types.QueryControl }

query_field:
  | QUERY_FIELD LPAREN field_ref COMMA const_expr RPAREN
  {
    let h,f = $3 in
    let w = int_of_string $5 in
    Query_Types.QueryField(h, f, w) }

query_field_exact:
  | QUERY_FIELD_EXACT LPAREN field_ref COMMA const_expr RPAREN
  {
    let h,f = $3 in
    let w = int_of_string $5 in
    Query_Types.QueryFieldExact(h, f, w) }

query_field_range:
  | QUERY_FIELD_RANGE LPAREN field_ref COMMA const_expr RPAREN
  {
    let h,f = $3 in
    let w = int_of_string $5 in
    Query_Types.QueryFieldRange(h, f, w) }

query_field_counter:
  | QUERY_FIELD_COUNTER LPAREN ID COMMA RPAREN
  {
    (* TODO: parse the second arg (tumble time) *)
    Query_Types.QueryFieldCounter($3, 10000) }


field_ref:
  | ID DOT ID
  { ($1, $3) }

const_expr:
  | NUMBER { $1 }

