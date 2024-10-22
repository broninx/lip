(* tokens *)
type token = A | B | X

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let to_token = function | 'A' ->  A | 'B' ->  B | '=' ->  X | _ -> failwith "it's not a word of the language" 
let toklist_of_string s = explode s |> List.map (fun g -> to_token g) 
  
(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let rec valid = function 
  | [] -> true
  |  A::t |  B::t |  X::t -> valid t  

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let rec count_element s = function 
  | [] -> 0
  | h::t when h ==s  -> 1+ count_element s t
  | _::t -> count_element s t 

let win l = match (count_element  A l) - (count_element  B l) with
| s when s>0 ->  A
| s when s<0 ->  B
| _ ->  X


(* val string_of_winner : token -> string *)
let string_of_winner = function
|  A -> "A"
|  B -> "B"
|  X -> "X"


