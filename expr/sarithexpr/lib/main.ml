open Ast

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Zero -> "Zero"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "not("^ string_of_expr e ^ ")"
  | And(e1,e2) -> string_of_expr e1 ^ " and "^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or "^ string_of_expr e2
  | IsZero(e) -> "iszero(" ^ string_of_expr e ^ ")"
  | Succ(e) -> "succ(" ^ string_of_expr e ^ ")"
  | Pred(e) -> "pred(" ^ string_of_expr e ^ ")"




let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


type exprtype = BoolT | NatT
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

let string_of_type = function
| NatT -> "Nat"
| BoolT -> "Bool"
exception TypeError of string
let rec typecheck : expr -> exprtype = function
  True -> BoolT
  | False -> BoolT
  | Zero -> NatT
  | Succ(e) when typecheck e = NatT -> NatT
  | Succ(e) -> raise (TypeError (string_of_expr e ^ " has type Nat, but Bool type was expected"))
  | Pred(e) when typecheck e = NatT -> NatT 
  | Pred(e) -> raise (TypeError (string_of_expr e ^ " has type Nat, but Bool type was expected"))
  | IsZero(e) when typecheck e = NatT -> BoolT
  | IsZero(e) -> raise (TypeError (string_of_expr e ^ " has type Bool, but Nat type was expected"))
  | Not(e) when typecheck e = BoolT -> BoolT
  | Not(e) -> raise (TypeError (string_of_expr e ^ " has type Nat, but Bool type has expected"))
  | And(e1,_) when typecheck e1 = NatT -> raise (TypeError (string_of_expr e1 ^ " has type Bool, but Nat type has expected"))
  | And(_,e2) when typecheck e2 = NatT -> raise (TypeError (string_of_expr e2 ^ " has type Bool, but Nat type has expected"))
  | And(_,_) -> BoolT
  | Or(e1,_) when typecheck e1 = NatT -> raise (TypeError (string_of_expr e1 ^ " has type Bool, but Nat type has expected"))
  | Or(_,e2) when typecheck e2 = NatT -> raise (TypeError (string_of_expr e2 ^ " has type Bool, but Nat type has expected"))
  | Or(_,_) -> BoolT
  | If(e0,_,_) when typecheck e0 = NatT -> raise (TypeError (string_of_expr e0 ^ " has type Nat, but Bool type has expected"))
  | If(_,e1,e2) when typecheck e1 != typecheck e2 -> 
      raise (TypeError (string_of_expr e2 ^ " has type "^ (typecheck e2 |> string_of_type) ^", but "^ (typecheck e1 |> string_of_type)^" type has expected"))
  | If(_,e1,_) -> typecheck e1
 
 
 
 






