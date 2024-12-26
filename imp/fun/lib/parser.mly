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

%token INTVAR 
%token FUN

%token SKIP
%token ASSIGN
%token SEQ
%token IF
%token WHILE


%token LPAREN "("
%token RPAREN ")"
%token LGPAREN "{"
%token RGPAREN "}"

%token THEN
%token ELSE
%token DO

%token RETURN 
%token EOF

%left SEQ
%left DO ELSE
%left AND OR NOT
%left ADD SUB EQ LEQ
%left MUL
%start <prog> prog

%%

prog:
  | d = list(decl);SEQ; c = cmd ; EOF { Prog(d,c) }
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
  | v = VAR; "("; e = expr; ")" { Call(v,e) }
  | "("; c = expr; ")" {c}
;

decl:
  | INTVAR; e2 = VAR ; { IntVar(e2) }
  | FUN; i1 = VAR;"("; i2 = VAR;")";"{"; c = cmd;RETURN; e = expr; "}" { Fun(i1,i2,c,e) }
;

cmd:
  | SKIP { Skip }
  | v = VAR; ASSIGN; e = expr { Assign(v,e) }
  | c1 = cmd; SEQ; c2 = cmd { Seq(c1,c2) }
  | IF; e1 = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e1, c1, c2) }
  | WHILE; e = expr; DO;  c = cmd { While(e,c) }
  | "("; c = cmd; ")" {c}
;