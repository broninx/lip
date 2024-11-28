%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token IF
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token LPAREN
%token RPAREN

%token THEN
%token ELSE


%token EOF

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2)}
  | e1 = expr; OR; e2 = expr; { Or(e1, e2)}
  | NOT; e1 = expr; { Not(e1)}
  | ZERO; { Zero }
  | SUCC; e1 = expr; { Succ(e1)}
  | PRED; e1 = expr; { Pred(e1)}
  | ISZERO; e1 = expr; { IsZero(e1)}
  | LPAREN; e=expr; RPAREN {e}
;

