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
  | h :: m :: [] when h='0' && m = '0'-> true  
  | h:: m  :: t when h = '0'-> (m = '0' || m = '1') && lang3 @@ h::t 
  | _ -> false

let rec lang4 = function 
  | h:: m :: [] when h= '1' && m = '1' -> true;
  | h:: m:: t when not ((List.filter (fun (x) -> not (x='1')) (h::m::t)) = []) ->
      lang4 @@ List.filter  (fun (x) -> x = '1') (h::m::t) 
  | _ -> false

let rec lang5 = function
  | h :: m :: [] when (h = '1' && m = '1') || (h = '0' && m = '0')-> true
  | h :: m :: t when  (h = '1' && m = '1') || (h = '0' && m = '0')-> lang5 t 
  | _ -> false
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
 
