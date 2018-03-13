(*Konstanty Kostrzewski*)
(*reviewer: Lukasz Goluchowski*)
type t =
  |Empty
  |Node of t * (int * int) * t * int * int;;
(* w kazdym wezle trzymam lewe poddrzewo, przedzial, prawe poddrzewo, wysokosc
drzewa o korzeniu w tym wezle i liczbe liczb naturalnych zawartych w tym drzewie
*)
let empty = Empty;;

let is_empty t = (t = Empty)

let height = function
  |Empty -> 0
  |Node(_, _, _, h, _) -> h;;

let num_of_elem = function
  |Empty -> 0
  |Node(_, _, _, _, k) -> k;;

let num w =
  if w >= 0 then w else max_int;; (* jak sie licznik przekreci to ma zwracac
max_inta - pomocnicze do num_of_elem *)

let make l (a, b) r =
  let elem = if b - a < 0 then max_int else
  num (num_of_elem l + num_of_elem r + b - a + 1) in
  Node(l, (a, b), r, max (height l) (height r) + 1, elem);;

let balance l ((a, b) as k) r =
  let hl = height l in
  let hr = height r in
  if hl >= hr + 2 then
    match l with
    |Node(ll, lk, lr, _, _) ->
	if height ll >= height lr then make ll lk (make lr k r)
	else
	  (match lr with
	   |Node (lrl, lrk, lrr, _, _) ->
	       make (make ll lk lrl) lrk (make lrr k r)
	   |Empty -> assert false)
    |Empty -> assert false
  else if hr >= hl + 2 then
     match r with
     |Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
     |Empty -> assert false
  else make l k r;;
exception Not_found_min;;
exception Not_found_max;;

let rec min_elt = function
  |Node(Empty, k, _, _, _) -> k
  |Node(l, _, _, _, _) -> min_elt l
  |Empty -> raise Not_found_min;;

let rec max_elt  = function
  |Node(_, k, Empty, _, _) -> k
  |Node(_, _, r, _, _) -> max_elt r
  |Empty -> raise Not_found_max;;

let rec remove_min_elt = function
  |Node(Empty, _, r, _, _) -> r
  |Node(l, k, r, _, _) -> balance (remove_min_elt l) k r
  |Empty -> invalid_arg "iSet.remove_min_elt";;

let rec remove_max_elt = function
  |Node(l, _, Empty, _, _) -> l
  |Node(l, k, r, _, _) -> balance l k (remove_max_elt r)
  |Empty -> invalid_arg "iSet.remove_max_elt";;

let merge t1 t2 =
  match t1, t2 with
  |Empty, Empty -> Empty
  |Empty, _ -> t2
  |_, Empty -> t1
  |_, _ ->
      let k = min_elt t2 in
      balance t1 k (remove_min_elt t2);;

let ( -- ) w v = if w = min_int then min_int else w - v;;

let rec add_one (x, y) t = (* jak nie jest "sasiedni" do danego wezla, czyli nie trzeba go z nim laczyc, to przerzucamy do odpowiedniego poddrzewa. W p. p. laczymy z wezlem i sprawdzamy czy nie trzeba jeszcze z pewnym wezlem w odpowiednim poddrzewie *)
  match t with
  |Empty -> make Empty (x, y) Empty
  |Node(Empty, (a, b), Empty, _, _) ->
      if y < a -- 1 then
	balance (add_one (x, y) Empty) (a, b) Empty
      else
	if x -- 1 > b then
	  balance Empty (a, b) (add_one (x, y) Empty)
	else
	  if y = a -- 1 then make Empty (x, b) Empty
	  else make Empty (a, y) Empty

  |Node(Empty, (a, b), r, _, _) ->
      if y < a -- 1 then make (make Empty (x, y) Empty) (a, b) r
      else
	if x -- 1 > b then balance Empty (a, b) (add_one (x, y) r)
	else
	  if y = a -- 1 then make Empty (x, b) r
	  else
	    let (u, v) = min_elt r in
              if u -- 1 = y then
		balance Empty (a, v) (remove_min_elt r)
	      else make Empty (a, y) r

  |Node(l, (a, b), Empty, _, _) ->
      if y < a -- 1 then balance (add_one (x, y) l) (a, b) Empty
      else
	if x -- 1 > b then make l (a, b) (make Empty (x, y) Empty)
	else
	  if y = a -- 1 then
	    let (u, v) = max_elt l in
	    if v = x -- 1 then
	      balance (remove_max_elt l) (u, b) Empty
	    else make l (x, b) Empty
	  else
	    make l (a, y) Empty

  |Node(l, (a, b), r, _, _) ->
      if y < a -- 1 then
	balance (add_one (x, y) l) (a, b) r
      else
	if x -- 1 > b then
	  balance l (a, b) (add_one (x, y) r)
	else
	  if y = a -- 1 then
	    let (u, v) = max_elt l in
	    if v = x -- 1 then
	      balance (remove_max_elt l) (u, b) r
	    else make l (x, b) r
	  else
	      let (u, v) = min_elt r in
	      if u -- 1 = y then
		balance l (a, v) (remove_min_elt r)
	      else make l (a, y) r;;

let rec join l x r =
  match (l, r) with
  |(Empty, _) -> add_one x r
  |(_, Empty) -> add_one x l
  |(Node(ll, lx, lr, lh, _), Node(rl, rx, rr, rh, _)) ->
      if lh > rh + 2 then
	balance ll lx (join lr x r)
      else
	if rh > lh + 2 then balance (join l x rl) rx rr
	else
	  make l x r;;

let split x t =
  let rec pom x t =
    match t with
    |Empty -> (Empty, false, Empty)
    |Node(l, (a, b), r, _, _) ->
	if x < a then
	  let (ll, pres, rl) = pom x l in (ll, pres, join rl (a, b) r)
	else
	  if x > b then
	    let (lr, pres, rr) = pom x r in (join l (a, b) lr, pres, rr)
	  else
	    let l =
	      if a < x then add_one (a, x - 1) l
	      else l
	    in
	    let r =
	      if b > x then add_one (x + 1, b) r
	      else r
	    in
       (l, true, r)
   in pom x t;;

let remove (a, b) t =
  let u = (split a t)  in
  let v = (split b t)  in
  match u, v with
  |(l, _, _), (_, _, r) ->
  merge l r;;

let add (x, y) t =
  add_one (x, y) (remove (x, y) t);;

let rec mem x t =
  match t with
  |Empty -> false
  |Node(l, (a, b), r, _, _) ->
      if a <= x && x <= b then true
      else
	if x < a then
	  mem x l
	else
	  mem x r;;

let rec iter f t =
  match t with
  |Empty -> ()
  |Node(l, (a, b), r, _, _) ->
      iter f l; f (a, b); iter f r;;

let rec fold f t c =
  match t with
  |Empty -> c
  |Node(l, (a, b), r, _, _) ->
      fold f r (f (a, b) (fold f l c))

let elements t =
  let rec pom tt ll =
    match tt with
    |Empty -> ll
    |Node(l, (a, b), r, _, _) ->
	pom l ((a, b)::(pom r ll)) in
  pom t [];;

let below x t =
  let rec pom x t c =
    match t with
    |Empty -> c
    |Node(l, (a, b), r, _, _) ->
    if x >= a && x <= b then
      num (num_of_elem l + c + x - a + 1)
    else
      if x < a then
	pom x l c
      else
	pom x r (num (c + b - a + 1  + (num_of_elem l)))
in
  let q = pom x t 0 in
if (q < 0) || (not (is_empty t) && q = 0 && x = max_int) then
  max_int
else q;;

(*
let a = add (0, 5) empty;;
let a = add (7, 8) a;;
let a = add (-3, -3) a;;
let a = add (10, 13) a;;
assert(elements a = [(-3, -3); (0, 5); (7, 8); (10, 13)]);;
assert(below 8 a = 9);;
let b = add (6, 6) a;;
let b = remove (6, 6) b;;
let b = add (-100, -5) b;;
let b = add (-4, 6) b;;
assert(elements b = [(-100, 8); (10, 13)]);;
assert(below 10 b = 110);;
let c = remove (2, 10) a;;
assert(elements c = [(-3, -3); (0, 1); (11, 13)]);;
assert(below 12 c = 5);;
print_endline "ok";;

let a = empty
let a = add (-20, 5) a
let a = add (6, 18) a
let a = add (4, 10) a
let a = add (14, 16) a
let a = remove (-18, 14) a
let a = remove (5, 17) a;;
assert(mem 14 a = false);;
let a = add (-4, 9) a;;
assert(mem 16 a = false);;
assert(mem (-14) a = false);;
assert(mem 10 a = false);;
let a = remove (-9, 10) a;;
let a = add (-6, 7) a;;
let a = add (-2, 7) a;;
let a = add (-12, 17) a;;
let a = add (-13, 8) a;;
let a = add (-13, -2) a;;
assert(mem 11 a = true);;
assert(elements a = [(-20, -19); (-13, 18)]);;
print_endline "ok";;
*)
