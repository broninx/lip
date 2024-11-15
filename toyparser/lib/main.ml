open Ast

(* parse : string -> ast *)


let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type int_or_err = (int, string) Result.t

let ( ==> ) (res : int_or_err) (f : int -> int_or_err) : int_or_err =
  match res with
  | Ok value -> f value
  | Error msg -> Error msg

let string_of_intorerr : int_or_err -> string = function
  | Ok n -> string_of_int n
  | Error msg -> msg

 (*--------------my work--------------*) 

(*int_of_digit: string -> int*)
let int_of_digit d = match d with
'0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 2
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'A' | 'a' -> 10
  | 'B' | 'b' -> 11
  | 'C' | 'c' -> 12
  | 'D' | 'd' -> 13
  | 'E' | 'e' -> 14
  | 'F' | 'f' -> 15
  | _ -> 0
let string_to_char_list s =
  s |> String.to_seq |> List.of_seq

  (*pow: int -> int -> int*)
let rec pow base exp = match exp with 
0 -> 1 
  | 1 -> base
  | _ -> base * base * pow base (exp-1)

(*int_of_exadecimal: string -> int*)

let int_of_exadecimal (e:string) : int =  
  let rec int_of_exadecimal_of_string acc = function
  | h::_ when h = 'x' || h = 'X' -> 0
  | h::t -> (int_of_digit h) * pow 16 acc  + (int_of_exadecimal_of_string  (acc+1) t)
  | _ -> 0
  in int_of_exadecimal_of_string 0 (string_to_char_list e |> List.rev) 
(*---------------------------------------------------------------------------------------*)
(* eval : ast -> result *)

let rec eval : ast -> int_or_err = function
  | Const n -> Ok n
  | Exconst d -> Ok (int_of_exadecimal d)
  | Add (e1,e2) ->
      eval e1 ==> fun v1 ->
      eval e2 ==> fun v2 ->
      Ok (v1 + v2)  
  | Minus (e1,e2) ->
      eval e1 ==> fun v1 -> 
      eval e2 ==> fun v2 ->
      Ok(v1-v2)
  | Mul (e1,e2) ->
      eval e1 ==> fun v1 -> 
      eval e2 ==> fun v2 ->
        Ok(v1*v2)
  | Div (e1,e2) -> 
      eval e1 ==> fun v1 -> 
      eval e2 ==> fun v2 -> match v2 with
      | 0 -> Error ("Error: tried to divide "^ string_of_int v1 ^" by zero")
      | _ -> Ok(v1/v2) 
    
