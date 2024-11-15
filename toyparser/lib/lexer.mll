{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let neg_num = '-'num
let exdig = ['a'-'f']|['A'-'F']|['0'-'9']
let exa_num = ['0'] (['X']|['x']) exdig exdig*  



rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  | num { CONST (Lexing.lexeme lexbuf) }
  | neg_num {CONST (Lexing.lexeme lexbuf) }
  | exa_num { EXACONST (Lexing.lexeme lexbuf)}
  | eof { EOF }
