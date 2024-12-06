{
open Parser
}

let white = [' ' '\t']+
let words = (['a'-'z']|['A'-'Z'])+ (['a'-'z']|['A'-'Z'])*
let var = words (['0'-'9']|words)*
let const = ('0' | ['1'-'9'] ['0'-'9']*)

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "skip" { SKIP }
  | ":=" { ASSIGN }
  | ";" { SEQ }
  | var { VAR(Lexing.lexeme lexbuf) }
  | const { CONST(Lexing.lexeme lexbuf) }
  | eof { EOF }
