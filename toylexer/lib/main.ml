open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

  (* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf


(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
 let frequency n l  = 
    List.map (fun x -> (x, (List.filter (fun y -> y=x) l |> List.length )) ) l 
    |> List.sort (fun (_, xn) (_, yn) -> yn-xn)
    |> List.sort_uniq (fun (x, xn) (y, yn) -> if (not (x=y))&&(xn=yn) then (-1) else yn-xn) 
    |> List.filteri (fun x _  -> x<n )

(*---------------------------------------------*)
(*that's only for the test*)
  let rec tokenize_test lexbuf =
  match Lexer.read_token_test lexbuf with 
  EOF -> [EOF]
  | t -> t::(tokenize_test lexbuf)

let lexer_test (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize_test lexbuf
  (*---------------------------------------------*)

  
