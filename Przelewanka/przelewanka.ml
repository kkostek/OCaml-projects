open Array
exception I_found_it (* hurrrrraaaa! :D *)

let przelewanka a =
  let n = length a in
  if n = 0 then 0 else (

  let wyn = ref 0 in
  let p = Hashtbl.create 3141 in (* mapa *)
  let q = Queue.create () in
  let y = init n (fun i -> snd a.(i)) in
  let ss = make n 0 in

  let dolej s i =
    let s = copy s in
    s.(i) <- fst a.(i);
    s
  in

  let wylej s i =
    let s = copy s in
    s.(i) <- 0;
    s
  in

  let przelej s i j =       (*przelej od i-tej do j-tej*)
    let s = copy s in
    if (fst a.(j)) - s.(j) >= s.(i) then (
      s.(j) <- s.(j) + s.(i);
      s.(i) <- 0;
      s)
    else (
      s.(i) <- s.(i) + s.(j) - (fst a.(j));
      s.(j) <- fst a.(j);
      s)
  in

  let rec nwd p q =
    if p = 0 then q
    else
      if q = 0 then p
       else
	if p = q then p
	else
	  if p > q then nwd q (p mod q)
	  else nwd q p
  in

  let d = fold_left (fun acc (x,_) -> nwd acc x) 0 a in (*NWD*)
  let solv t1 = (t1 = y) in
  (*warunki*)
  if d = 0 then 0
  else (
  let war1 = fold_left (fun acc (_, y) -> (acc && (y mod d = 0))) true a in
  let war2 = fold_left (fun acc (x, y) -> (acc || y = 0 || y = x)) false a in
  Queue.add ss q;
  Queue.add [||] q;
  if not war1 || not war2 then -1
   else
      (* jak przeszlo oba warunki *)
	try
	  while not (Queue.is_empty q) do
	    let h = Queue.pop q  in

	    if solv h then raise I_found_it (*cieszymy sie*)
	    else
	      if not (Hashtbl.mem p h) then
		begin
		  if h = [||] then
		    if not (Queue.is_empty q) then (
		      print_newline ();
		      wyn := !wyn + 1;
		      Queue.add h q )
		    else ()
		  else
		    for i = 0 to n-1 do
		      Hashtbl.add p h !wyn;
		      Queue.add (wylej h i) q;
		      Queue.add (dolej h i) q;
		      for j = 0 to n - 1 do
			if i <> j then
			  Queue.add (przelej h i j) q;
		      done
		    done;
		end;
          done; -1
	with I_found_it -> !wyn
   ))
;;
