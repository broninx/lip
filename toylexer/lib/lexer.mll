{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let atok = ['0'-'9' 'A'-'Z'] letter*
let vowel = ['a' 'e' 'i' 'o' 'u']
let cvowel = ['A' 'E' 'I' 'O' 'U']
let btok  = vowel*
let lwvowel = letter # vowel # cvowel (*letter without vowel*)
let ctok = lwvowel* (cvowel|vowel)? lwvowel* 
let dtok = ['-'] num* ['.'] ['0'-'9']*
let exdig = ['a'-'f']|['A'-'F']|['0'-'9']
let etok = ['0'] (['X']|['x']) (exdig # '1') (exdig)*  

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
  | atok { ATOK (Lexing.lexeme lexbuf) }
  | btok { BTOK (Lexing.lexeme lexbuf)}
  | ctok { CTOK (Lexing.lexeme lexbuf)}
  | dtok { DTOK (Lexing.lexeme lexbuf)}
  | etok { ETOK (Lexing.lexeme lexbuf)}   
  

and read_token_test =
  parse
  | white { read_token lexbuf }  
  | eof { EOF }
  | atok { ATOK (Lexing.lexeme lexbuf) }
  | btok { BTOK (Lexing.lexeme lexbuf)}
  | ctok { CTOK (Lexing.lexeme lexbuf)}
  | dtok { DTOK (Lexing.lexeme lexbuf)}
  | etok { ETOK (Lexing.lexeme lexbuf)}
 
