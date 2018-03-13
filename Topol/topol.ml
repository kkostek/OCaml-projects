exception Cykliczne
open PMap
let topol g =
  match g with
  |[] ->[]
  |h::t -> (
  let res = ref [] in
  let l = ref empty and n = ref empty in
  List.iter (fun (v,lv) -> n := add v lv !n) g;
  let rec dfs v =
    if not (exists v !l) then (
      l := add v false !l;
      List.iter dfs (try find v !n with Not_found -> []);
      l := remove v !l;
      l := add v true !l;
      res := v::(!res)        )
    else
      if not (find v !l) then raise Cykliczne in
  List.iter (fun (v, _) -> dfs v) g;
  !res);;
