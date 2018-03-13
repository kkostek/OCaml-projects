(*Reviewer: Wojciech Matusiak*)
type wartosc = float * float * bool
(* bool = true -> przedzial reprezentuje przedzial zawierajacy najmniejszy i najwiekszy element; bool = false -> przedzial jest suma przedzialow od neg_infinity do lewy i od prawy do infinity *)

    let lewy (x, _, _) = x;;

    let prawy (_, y, _) = y;;

    exception Zla_dana of float;;
    exception Zle_dane of float;;

    let wartosc_dokladnosc x p =
      if p > 0. then
	if x >= 0. then
	  let ll = x -. x *. p /. 100. in
	  let pp = x +. x *. p /. 100. in
            (ll, pp, true)
	else
	  let ll = x +. x *. p /. 100. in
	  let pp = x -. x *. p /. 100. in
	    (ll, pp, true)
      else raise (Zla_dana p);;

    let wartosc_od_do x y =
      if x <= y then (x, y, true)
      else raise (Zle_dane  y);;

    let wartosc_dokladna x =
      (x, x, true);;

    let in_wartosc w x =
      match w with
      | (l, p, true) -> (x >= l && x <= p)
      | (l, p, false) -> ( x <= l || x>= p);;

    let min_wartosc w =
      match w with
      | (l, _, true) -> l
      | (_, _, false) -> neg_infinity;;

    let max_wartosc w =
      match w with
      | (_, p, true) -> p
      | (_, _, false) -> infinity;;

    let sr_wartosc (x, y, b) =
	if b then
      0.5 *. (max_wartosc (x, y, b) +. min_wartosc (x, y, b))
       	else nan;;

    let r = (neg_infinity, infinity, true);; (* to jest przedzial rowny calej osi rzeczywistej *)

(* konwencja: else do danego ifa pisze z takim samym wcieciem co danego ifa. *)

       let rec plus (x, y, b1) (z, t, b2) =
	 if b1 && b2 then (x +. z, y +. t, true)
	 else
	   if b1 && not b2 then
	     if x +. t > y +. z then (y +. z, x +. t, false)
	     else r
	   else
	     if not b1 && b2 then plus (z, t, b2) (x, y, b1)
	     else r;;


       let minus (x, y, b1) (z, t, b2) =
	 let ujemny (a1, a2, b) = ((-. a2), (-. a1), b)
	 in
	    plus (x, y, b1) (ujemny (z, t, b2));;


       let rec razy (x, y, b1) (z, t, b2) =
        let min a b = if a <= b then a else b in
        let max a b = if a >= b then a else b in

	 if ((x, y, b1) = r || (z, t, b2) = r) then r
         else
           if b1 && b2 then
              if x >= 0. && z >= 0. then (x *. z, y *. t, true)
              else
                if y <= 0. && t <= 0. then (y *. t, x *. z, true)
                else
                  if x >= 0. && t <= 0. then (y *. z, x *. t, true)
                  else
                    if z >= 0. && y <= 0. then (x *. t, y *. z, true)
                    else
                      if x *. y <= 0. && z *. t >= 0. then
                        if t > 0. then (x *. t, y *. t, true)
                        else (y *. z, x *. z, true)
                      else
                        if x *. y >= 0. && z *. t <= 0. then
                          if y > 0. then (z *. y, t *. y, true)
                          else (t *. x, z *. x, true)
                        else
                          (min (x *. t) (y *. z), max (x *. z) (y *. t), true)
         else
	  if b1 && not b2 then
            if x >= 0. then
	      if z *. t < 0. then (z *. x, x *. t, false)
	      else
	        if z >= 0. then
	          if z *. y < x *. t then (z *. y, x *. t, false)
	          else r
	        else
 		    if z *. x < y *. t then (z *. x, y *. t, false)
		    else r
	    else
	      if y <= 0. then
	        if z *. t < 0. then (t *. y, z *. y, false)
	        else
	          if z >= 0. then
		    if z *. x > y *. t then (y *. t, z *. x, false)
		    else r
	  	  else
		    if t *. x < y *. z then (t *. x, y *. z, false)
		    else r
              else r



      else
       if not b1 && b2 then razy (z, t, b2) (x, y, b1)
       else
  	   if x *. y < 0. && z *. t < 0. then
            (max (x *. t) (y *. z), min (x *. z) (y *. t), false)
         else r;;

       let podzielic (x, y, b1)  (z, t, b2) =
         let odwrotnosc (k, l, b) =
	   if b then
	     if k *. l > 0. then ((1. /. l), (1. /. k), true)
	     else
		if k *. l < 0. then ((1. /. k),  (1. /. l), false)
		else
		    if k = 0. then
			if l = 0. then r
            		else ( (1. /. l) , infinity, true)
		    else (neg_infinity, (1. /. k), true)
	   else
	       if k *. l < 0. then ((1. /. k), (1. /. l), true)
	       else ((1. /. l), (1. /. k), false)
	 in
           razy (x, y, b1) (odwrotnosc (z, t, b2));;

(*
assert(razy a b = ((-8.), 6., true));
assert(plus (3., 4., true) ((-2.), 4., false) = (2., 7., false));
assert(podzielic a b = ((-1.5), 2., true));
assert(podzielic a (1., 3., true) = ((-3.), 4., true));
assert(razy ((-1.), 3., false) ((-1.), 3., false) = ((-3.), 1., false));
*)
