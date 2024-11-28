open Ast

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Zero -> "Zero"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "not "^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and "^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or "^ string_of_expr e2
  | IsZero(e) -> "iszero " ^ string_of_expr e
  | Succ(e) -> "succ " ^ string_of_expr e
  | Pred(e) -> "pred " ^ string_of_expr e




let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast



type exprval = Nat of int | Bool of bool

let string_of_val = function 
  Nat e -> string_of_int e
  | Bool true -> "true"
  | Bool false -> "false"

let int_of_Nat e = match e with | Nat n when n >= 0 -> n | _ -> failwith "isn't natural number"

let bool_of_Bool e = match e with Bool true -> true | Bool false -> false | _ -> failwith "isn't boolean value"

exception NoRuleApplies

let rec trace1 = function
    If(True,e1,_) -> trace1 e1
  | If(False,_,e2) -> trace1 e2
  | If(e0,e1,e2) -> If(trace1 e0, e1, e2) 
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> Not(trace1 e)
  | And(True, e2) -> And(True, trace1 e2)
  | And(False, _) -> False
  | And(e1, e2) -> And(trace1 e1, e2)
  | Or(True, _) -> True
  | Or(False, e2) -> Or(False, trace1 e2)
  | Or(e1, e2) -> Or(trace1 e1, e2)
  | Pred(Succ(e)) -> e
  | Succ(e) -> Succ(trace1 e)
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e) -> IsZero(trace1 e)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval : expr -> exprval = function
  | True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | Not(e) -> Bool (not (bool_of_Bool (eval e))) 
  | If(b,c,_) when eval b = Bool true -> eval c
  | If(b, _, c) when eval b = Bool false -> eval c
  | If(_,_,_) -> failwith "mmh"
  | Succ(e) ->   Nat ( 1 + int_of_Nat (eval e))
  | Pred(e) ->  Nat (int_of_Nat (eval e) - 1)
  | And(e1, e2) -> let b1 = bool_of_Bool (eval e1) in let b2 = bool_of_Bool (eval e2) in Bool (b1 && b2) 
  | Or(e1, e2) -> let b1 = bool_of_Bool (eval e1) in let b2 = bool_of_Bool (eval e2) in Bool (b1 || b2) 
  |IsZero(e) -> if ((int_of_Nat (eval e)) = 0 )then Bool true else Bool false


let is_nv = function
  | Zero -> true
  | Succ(_) -> true
  | Pred(e) when eval e != Nat 0 -> true
  | _ -> false





