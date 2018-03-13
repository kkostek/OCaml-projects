type point = float * float;;
type kartka = point -> int;;

let x (a, _) = a;;
let y (_, b) = b;;
let square x = x *. x;;
let dist2 (a1, a2) (b1, b2) =
     square (b1 -. a1) +. square (b2 -. a2);; 
let e = 0.000001;; (*  *)

let cross_product p q r = 
(* cross product of vectors  pq and pr*) 
    (y r -. y p) *. (x q -. x p) -. (y q -. y p) *. (x r -. x p);;
   
let punkt_sym ((r1, r2) as r) ((p1, p2) as p) ((q1, q2) as q) =
(* point symmetric to r with respect to line pq *)
(* rotation through an angle 2 * alpha, where alpha is as follows*)
(* alpha - angle formed by lines pq and pr*)
  let u = sqrt (dist2 p r) in
  let v = sqrt (dist2 p q) in
  let w = sqrt (dist2 q r) in
  let alpha = acos ((square u +. square v -. square w) /. (2. *. u *. v))  in 
  (* law of cosines *)
  let s = -. sin (2.*. alpha) in
  let x1 = (r1 -. p1) *. cos (2. *. alpha) -. (r2 -. p2) *. s +. p1 in
  let x2 = (r1 -. p1) *. s +. (r2 -. p2) *. cos (2. *. alpha) +. p2 in
  (x1, x2);;

let prostokat ((a1, a2):point) ((b1, b2):point) =
  (fun ((p1, p2):point) -> if p1 >= a1 -. e && p1 <= b1 +. e  && p2 >= a2 -. e && p2 <= b2 +. e then 1 else 0
  );;

let kolko ((a1, a2) as a) r = 
  (fun ((p1, p2) as p) -> if dist2 a p <= square (r +. e) then 1 else 0);;

let zloz p q k= 
  fun r -> if abs_float (cross_product p r q) < e then k r else 
          if cross_product p q r < 0. then 0
	  else k r + k (punkt_sym r p q);;

let skladaj l k = 
  let f a (p1, p2) = zloz p1 p2 a in
  List.fold_left f k l;;
