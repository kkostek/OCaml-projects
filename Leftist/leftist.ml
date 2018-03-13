type 'a queue = | Node of 'a * int * 'a queue * 'a queue | Leaf;;
(* value, dlugosc_skrajnie_prawej_sciezki, poddrzewo_lewe, poddrzewo_prawe *)

let empty = Leaf;;

let is_empty a = 
	if a = Leaf then true else false;;

let rd q =  (* zwraca dlugosc skrajnie prawej sciezki *)
	match q with
	| Leaf -> -1
	| Node(_, d, _, _) -> d;;

exception Empty;;

let rec join q1 q2 =
	match q1, q2 with
	| Leaf, _ -> q2
	| _, Leaf -> q1
	| Node(v1, _, l1, r1), Node(v2, _, _, _) ->
	        if v1 <= v2 then 
		 let a = join r1 q2 in
			if rd l1 <= rd a then
			     Node(v1, rd (l1) + 1, a, l1) 
			else 
			  Node(v1, (rd a) + 1, l1, a)
		else
		        join q2 q1;;

let add v q = 
  join (Node(v, 0, Leaf, Leaf)) q;;

let delete_min q = 
  match q with
  |Node(v, d, l, r) -> (v, join l r)
  |Leaf -> raise Empty;;
