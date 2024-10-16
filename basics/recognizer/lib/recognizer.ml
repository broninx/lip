let rec lang1  = function
  | [] -> false
  | h :: [] -> (h='1' || h = '0')
  | h:: t -> (h='1' || h = '0') && lang1 t 

let rec lang2  = function 
  | [] -> true
  | h::[] when h = '1' || h='0' -> true
  | h::m::t when h='0' || h='1' -> m='1' && lang2 @@ m::t 
  | _ -> false

let rec lang3 = function
  | '0' :: '0' :: [] -> true  
  | '0':: '0'  :: t| '0' :: '1' :: t -> lang3 @@ '0'::t 
  | _ -> false

let rec ch01 = function
  | '0' :: [] -> true 
  | '0':: t -> ch01 t
  | _ -> false

let rec lang4 = function 
  | '1':: '1' :: [] -> true;
  | h:: m:: t when List.filter (fun (x) -> not (x='1')) (h::m::t) |> ch01 ->
      lang4 @@ List.filter  (fun (x) -> x = '1') (h::m::t) 
  | _ -> false

let rec lang5 = function
  | h :: m :: [] when (h = '1' && m = '1') || (h = '0' && m = '0')-> true
  | h :: m :: t when  (h = '1' && m = '1') || (h = '0' && m = '0')-> lang5 t 
  | _ -> false
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
 
