open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


let bind st x v = 
  fun y -> if x = y then v else st y

let ( ==> ) (res : exprval) (f : int -> exprval)  : exprval =
  match res with
  | Nat value -> f value
  | _ -> raise (TypeError "need nat")

let ( -=> ) (res : exprval) (f : bool -> exprval)  : exprval =
  match res with
  | Bool value -> f value
  | _ -> raise (TypeError "need bool")

let ( --> ) (res : conf) (f : conf -> conf)  : conf =
  match res with
  | St st -> f (St st) 
  | Cmd(c,s) -> f (Cmd(c,s))

let rec eval_expr (st : state) (exp: expr): exprval = match exp with 
  | True -> Bool true
  | False -> Bool false
  | Var(s) -> (st s)
  | Const(c) -> Nat c
  | Not(e) -> eval_expr st e -=> fun v -> Bool (not v) 
  | And(e1, e2) ->  
      eval_expr st e1 -=> fun v1 -> 
      eval_expr st e2 -=> fun v2 ->
          Bool (v1 && v2)
  | Or(e1, e2) ->  
      eval_expr st e1 -=> fun  v1 -> 
      eval_expr st e2 -=> fun v2 ->
          Bool (v1 || v2)
  | Add(e1, e2) ->  
      eval_expr st e1 ==> fun  v1 -> 
      eval_expr st e2 ==> fun  v2 ->
          Nat (v1 + v2)
  | Sub(e1, e2) ->  
      eval_expr st e1 ==> fun v1 -> 
      eval_expr st e2 ==> fun v2 ->
          Nat (v1 - v2)
  | Mul(e1, e2) ->  
      eval_expr st e1 ==> fun v1 -> 
      eval_expr st e2 ==> fun v2 ->
          Nat (v1 * v2)
  | Eq(e1, e2) ->  
      eval_expr st e1 ==> fun v1 -> 
      eval_expr st e2 ==> fun v2 ->
          Bool (v1 = v2)
  | Leq(e1, e2) ->  
      eval_expr st e1 ==> fun v1 -> 
      eval_expr st e2 ==> fun v2 ->
          Bool (v1 <= v2)


let bot = fun x -> raise (UnboundVar x)

let rec trace1 : conf -> conf = function
  | Cmd(Skip, st) -> St st 
  | Cmd(Assign(e1,e2), st) -> St (bind st e1 (eval_expr st e2 ))
  | Cmd(Seq(c1, c2), st) -> 
      (trace1 (Cmd(c1, st))--> function 
        | St st' ->  Cmd(c2,st')
        | Cmd(c1', st' ) -> Cmd(Seq(c1', c2), st') )  
  | Cmd(If(e,c1,c2), st) -> (match eval_expr st e with
      | Bool false -> Cmd(c2,st) 
      | Bool true -> Cmd(c1, st) 
      | _ -> raise (UnboundVar "the expression have some variable not decrared" ))
  | Cmd(While(e, c), st) -> begin match eval_expr st e with
    | Bool false -> St st
    | Bool true -> Cmd( (Seq(c, While(e, c))) , st) 
    | _ -> raise (UnboundVar "the expression have some variable not decrared" ) 
  end
  | _ -> raise NoRuleApplies

let  trace n t= let rec trace_rec n1 t1 = (match n1 with
  0 -> [t1]
  | num  when num > 0-> 
     ( try 
        let t' = trace1 t1 in 
        t1::(trace_rec (num-1) t') 
      with NoRuleApplies -> [t1] 
     )
  | _ -> [t1]) in (trace_rec n (Cmd(t,bot)))  





