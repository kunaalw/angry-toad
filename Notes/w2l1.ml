(* 
   curriedAdd - basically invokes two functions - the first one is x - which 
   then calls the function y - which adds x+y). So basically, curriedAdd 3 4
   = (curriedAdd 3) 4
*)

let rec incLst l =
  match l with
    [] -> []
  | x::xs -> (x+1)::(incLst xs)

let rec swapLst l = 
  match l with
    [] -> []
  | (x,y)::xs -> (y,x)::(swapLst xs)

(*
  OCaml's map function generalizes this pattern:
  invokes a user-defined function
*)  
