%{
open Ast
%}

%token TRUE
%token FALSE
%token <string> VAR
%token <string> CONST
%token NOT
%token AND
%token OR
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ

%token SKIP
%token ASSIGN
%token SEQ
%token IF
%token WHILE
%token LPAREN
%token RPAREN

%token THEN
%token ELSE
%token DO


%token EOF

%left SEQ
%left DO ELSE
%left AND OR NOT
%left ADD SUB EQ LEQ
%left MUL
%start <cmd> prog

%%

prog:
  | e = cmd ; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | e = VAR { Var(e) }
  | e = CONST { Const(int_of_string e) }
  | NOT; e1 = expr { Not(e1)}
  | e1 = expr; AND; e2 = expr { And(e1,e2) }
  | e1 = expr; OR; e2 = expr { Or(e1,e2) }
  | e1 = expr; ADD; e2 = expr { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1,e2) }
;

cmd:
  | SKIP { Skip }
  | e1 = VAR; ASSIGN; e2 = expr { Assign(e1,e2) }
  | e1 = cmd; SEQ; e2 = cmd { Seq(e1,e2) }
  | IF; e1 = expr; THEN; e2 = cmd; ELSE; e3 = cmd { If(e1, e2, e3) }
  | WHILE; e1 = expr; DO; e2 = cmd { While(e1,e2) }
  | LPAREN; e= cmd; RPAREN {e}
;

