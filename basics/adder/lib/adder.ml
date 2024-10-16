(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let rec addlist = function
  | [] -> 0
  | a :: [] -> a
  | a :: t -> a + addlist t
