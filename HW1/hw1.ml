(* Problem 01 *)
let rec fib (n:int) : int =
  match n with
        0 -> 0
      | 1 -> 1
      | _ -> fib(n-1) + fib(n-2) ;;


(* Problem 02 *)
let rec clone ((e, n) : 'a * int) : 'a list =
	match (e, n) with
    	(_, 0) -> []
      | (_, 1) -> [e]
      | (x, y) -> x::(clone(x,y-1)) ;;


(* Problem 03 *)
let rec fibsFromHelp (n, m, ret) : int list =
  match (n,m,ret) with 
      (0,_,_)                 -> [0]
    | (1,_,_)                 -> [1;0]
    | (_,0,_)                 -> (fibsFromHelp (n, (m+1), [0]))
    | (_,1,_)                 -> (fibsFromHelp (n, (m+1), [1;0]))
    | (_,_,retx::rety::rets)  -> if n>=m 
                                 then (fibsFromHelp (n, (m+1), (retx+rety)::ret))
                                 else ret

let rec fibsFrom (n:int) : int list =
  (fibsFromHelp (n, 0, [0])) ;;


(* Problem 04 *)
let rec concat l1 l2 =
  match l1 with
      [] -> l2
    | x::xs -> x::(concat xs l2)

let rec rev (l: 'a list) : 'a list =
    match l with
      [] -> []
    | x::xs -> (concat (rev xs) [x]);;


(* Problem 05 *)
let rec revHelper (remain, sofar) =
  match remain with
      []     -> sofar
    | [xa]   -> xa::sofar
    | xb::xs -> (revHelper (xs, xb::sofar))


let fastRev (l : 'a list) : 'a list =
  revHelper(l, []) ;;


(* Problem 06 *)
let rec tails (l : 'a list) : 'a list list =
	match l with
        [] -> [[]];
      | x::xs -> l :: (tails xs) ;;


(* Problem 07 *)
let rec split (l : 'a list) : 'a list * 'a list =
  match l with
      []         -> ([], [])
    | x::[]      -> ([x], [])
    | xa::xb::xs ->
       match (split xs) with
         (l1,l2) -> (xa::l1, xb::l2) ;;


(* Problem 08 *)
let rec count (v: 'a) (l: 'a list) : int =
  match (v, l) with
      (_,[])    -> 0
    | (x,y::ys) -> if x=y then ((count x ys)+1) else (count x ys) ;;


(* Problem 09 *)
let rec last (l: 'a list) : 'a option =
  match l with
      []    -> None
    | x::[] -> Some x
    | x::xs -> (last xs) ;;


(* Problem 10 *)
let rec flatten (l: 'a list list) : 'a list =
    match l with
        []    -> []
      | x::xs -> x @ (flatten xs) ;;


(* Problem 11 *)
let rec rmDups (l: 'a list) : 'a list = 
   match l with
       [] -> []
     | x::[] -> [x]
     | xa::xb::xs -> if xa=xb then (rmDups (xb::xs)) else (xa::(rmDups (xb::xs))) ;;


(* Problem 12 *)
let rec encode (l: 'a list) : (int * 'a) list =
  match l with 
      [] -> []
    | x::[] -> [(1,x)]
    | xa::xb::xs -> if xa=xb then match (encode (xb::xs)) with
                                    (ya::yb) -> match ya with
                                                  (za,zb) -> [((za+1), zb)]@yb
                                                 
                             else ([(1,xa)]@(encode (xb::xs))) ;;


(* Problem 13 *)
let rec drop_wc (l, k, m) : 'a list = 
  match (l,k,m) with 
      ([],ka,ma)    -> []
    | (xa::[],_,1)  -> []
    | (xa::[],_,_)  -> [xa]
    | (xa::xs,_,1)  -> (drop_wc (xs, k, k))
    | (xa::xs,_,_)  -> [xa]@(drop_wc (xs, k, (m-1)))

let drop (l: 'a list) (k: int) : 'a list = 
  (drop_wc (l, k, k)) ;;


(* Problem 14 *)
let rec sort (l: int list) : int list =
  match l with
      []    -> []
    | x::[] -> [x]
    | xa::xb::[] -> if xa>xb then (xb::[xa]) else (xa::[xb])
      

(* Problem 15 *)
let rec dec2bin (n: int) : int list = 
  match n with
      0 -> [0]
    | 1 -> [1]
    | _ -> (dec2bin ((n - (n mod 2))/2)) @ [(n mod 2)];;
