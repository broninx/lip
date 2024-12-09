open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let bot_env = fun x -> raise (UnboundVar x)
let bot_mem = fun (x: loc) -> raise (UnboundVar (string_of_int x))

let bind_mem m l me = 
  fun y -> if l = y then me else m y

let bind_env (e:env) (i: ide) (en: envval)= 
  fun y -> if i = y then en else e y


let ( ==> ) (res : memval) (f : int -> memval)  : memval =
  match res with
  | Int value -> f value
  | _ -> raise (TypeError "need int")

let ( -=> ) (res : memval) (f : bool -> memval)  : memval =
  match res with
  | Bool value -> f value
  | _ -> raise (TypeError "need bool")

let loc_of_envval = function
  | IVar i -> i
  |BVar b -> b

let rec rightEnv (st: state) (var: ide) : envval = 
  try 
    (topenv st) var 
  with UnboundVar _ -> rightEnv (popenv st, getmem st, getloc st) var

let hastypeboolan = function
  | Bool _ -> true
  | _ -> false
  
let hastypeinteger = function
  | Int _ -> true
  | _ -> false

let rec eval_expr (st : state) (exp: expr): memval =  
  match exp with 
  | True -> Bool true
  | False -> Bool false
  | Var(s) -> (getmem st)(rightEnv st s |> loc_of_envval)
  | Const(c) -> Int c
  | Not(e) -> begin match eval_expr st e with
        | Bool b -> Bool (not b)
        | _ -> raise (TypeError "this expression expected bool type but have int type")
  end
  | And(e1, e2) -> 
      eval_expr st e1 -=> fun v1 -> 
      eval_expr st e2 -=> fun v2 ->
          Bool (v1 && v2)
  | Or(e1, e2) ->  
      eval_expr st e1 -=> fun  v1 -> 
      eval_expr st e2 -=> fun v2 ->
          Bool (v1 || v2)
  | Add(e1, e2) ->  
      eval_expr st e1 ==> fun v1 -> 
      eval_expr st e2 ==> fun v2 ->
          Int (v1 + v2)
  | Sub(e1, e2) ->  
      eval_expr st e1 ==> fun v1 -> 
      eval_expr st e2 ==> fun v2 ->
          Int (v1 - v2)
  | Mul(e1, e2) ->  
      eval_expr st e1 ==> fun v1 -> 
      eval_expr st e2 ==> fun v2 ->
          Int (v1 * v2)
  | Eq(e1, e2) ->  
      eval_expr st e1 ==> fun v1 -> 
      eval_expr st e2 ==> fun v2 ->
          Bool (v1 = v2)
  | Leq(e1, e2) ->  
      eval_expr st e1 ==> fun v1 -> 
      eval_expr st e2 ==> fun v2 ->
          Bool (v1 <= v2)


let rec trace1 : conf -> conf = function
  | St s -> St s
  | Cmd(Skip, st) -> St st 
  | Cmd(Assign(e1,e2), st) -> begin 
    match (rightEnv st e1) with
    | BVar b when hastypeboolan (eval_expr st e2) -> 
        St ( getenv st , bind_mem (getmem st) b (eval_expr st e2) , (getloc st) ) 
    | IVar i when hastypeinteger (eval_expr st e2) -> 
        St ( getenv st , bind_mem (getmem st) i (eval_expr st e2) , (getloc st) ) 
    |  _ -> raise (TypeError ( "expected another type"))
  end
  | Cmd(Seq(c1, c2), (env,mem,loc)) -> (match (trace1 (Cmd(c1, (env,mem,loc)))) with 
    | St (env',mem',loc') when (List.length env' > List.length env) -> trace1 (Cmd(c2,(env,mem',loc'))) 
    | St st' -> trace1 (Cmd(c2, st'))
    | Cmd(c1', st' ) -> trace1 (Cmd(Seq(c1', c2), st') ))
  | Cmd(If(e,c1,c2), st) -> (match eval_expr st e with
    | Bool false -> Cmd(c2,st) 
    | Bool true -> Cmd(c1, st) 
    | _ -> raise (UnboundVar "the expression have some variable not decrared" ))
  | Cmd(While(e, c), st) -> begin match eval_expr st e with
    | Bool false -> St st
    | Bool true -> Cmd( (Seq(c, While(e, c))) , st) 
    | _ -> raise (UnboundVar "the expression have some variable not decrared" ) 
  end 
  | Cmd(Decl(d1, c1), st) -> trace1 (Cmd(Block(Decl(d1,c1)), ((bot_env)::(getenv st), getmem st, getloc st)))
  | Cmd(Block(Decl(d1, c1)), st) -> (
    match d1 with
    | EmptyDecl -> trace1 (Cmd(c1, st))
    | IntVar(v,l) -> trace1 (Cmd(Block(Decl(l, c1)), ( (bind_env(topenv st) v (IVar (getloc st)))::(popenv st),getmem st, (getloc st) +1)))
    | BoolVar(v,l)-> trace1 (Cmd(Block(Decl(l, c1)), ( (bind_env(topenv st) v (BVar (getloc st)))::(popenv st),getmem st, (getloc st) +1)))
  )
  | Cmd(Block(_), _) -> failwith ""
  

let  trace n t= let rec trace_rec n1 t1 = (match n1 with
  0 -> [t1]
  | num  when num > 0->  let t' = trace1 t1 in t1::(trace_rec (num-1) t') 
  | _ -> [t1]) in trace_rec n (Cmd(t, ([bot_env], bot_mem, 0)))  
