%{
open Ast
%}

%token <string> CONST
%token <string> EXACONST
%token PLUS
%token MINUS
%token MUL
%token DIV
%token LPAREN
%token RPAREN
%token EOF

%left PLUS MINUS
%left MUL DIV


%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | n = EXACONST { Exconst(n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Minus(e1,e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;
