module type Coord =
sig
  type dot
  val zero : dot
  val (++) : dot -> dot -> dot
  val (--) : dot -> dot -> dot
  val d_abs : dot->dot
  val distance : dot*dot -> dot*dot -> dot
  val dump_coord : dot*dot -> unit
end

module Coord_R : Coord with type dot = int =
struct
  type dot = int
  type coord = dot * dot

  let zero = 0

  let (++) d1 d2 = d1 + d2
  let (--) d1 d2 = d1 - d2

  let d_abs d = abs d

  (**retourne la distance de Manhattan entre c1 et c2*)
  let distance c1 c2 = match c1 with
  |(x1,y1) -> match c2 with
    |(x2,y2) -> (abs (x1-x2)) + (abs (y1 -y2))

  let dump_coord c = match c with
(x,y) ->
    Printf.printf "(%d,%d) " x y
end

module Coord_E : Coord with type dot = float =
struct
type dot = float
type coord = dot * dot

let zero = 0.0

let (++) d1 d2 = d1 +. d2
let (--) d1 d2 = d1 -. d2
let d_abs d = abs_float d
let distance c1 c2 = match c1 with
|(x1,y1) -> match c2 with
  |(x2,y2) -> ((d_abs (x1--x2)**2.) ++ (d_abs (y1 --y2)**2.))**0.5

let dump_coord c = match c with
(x,y) ->
    Printf.printf "(%f,%f) " x y
end

module FoncteurTree(X : Coord) =
struct

  type dot = X.dot
  type coord = dot * dot

  type tree = Noeud of bool * coord * tree list

  let (++) = X.(++)

  let zero = X.zero

  let distance = X.distance

  let dump_coord = X.dump_coord

  let dump l =
    let _ = Printf.printf "{ " in
    let _ = List.iter dump_coord l in
    Printf.printf "}\n%!"

  let dump_coord_tree t = match t with
  Noeud(_,c,_) -> dump_coord c

  let dump_coord_tree_list tl = Printf.printf "{ "; List.iter dump_coord_tree tl; Printf.printf " }"

  (** renvoie true si i n'est pas dans l *)
  let rec mem l i=match l with
  |[]->true
  |p::q->if i=p then false else mem q i

  let rec isin e l = match l with
  []->false
  |t::q -> if t = e then true else isin e q

  let rec uniq l = match l with
    [] -> []
    |t::q -> if not (mem q t) then uniq q else t::(uniq q)

  let rec remove_last l = match l with
  []->[]
  |t::q -> if q = [] then [] else t::(remove_last q)

  (**renvoie la distance entre la coord c et l'arbre t *)
  let distance_to_tree c t = match t with
    Noeud(_,c2,tl) -> distance c c2
  
  (** renvoie la somme des distances entre la coord c et tous les abres de la liste tl*)
  let rec distance_tree_list c tl = match tl with
  []->zero
  |t::q -> (distance_to_tree c t) ++ (distance_tree_list c q)

  (** renvoie la somme des poids des arrêtes de de l'arbre t, marche pas si t est cyclique*)
  let rec weight t = match t with
    Noeud(_,c,tl) -> distance_tree_list c tl ++ weight_list tl
  (**renvoie la somme des poids des arrêtes des arbres de la liste tl, marche pas si un élément de t est cyclique*)
  and weight_list tl = match tl with
  []->zero
  |t::q -> weight t ++ weight_list q

  (**renvoie la liste des abscisses d'un arbre*)
  let getabsciss t = let rec aux_abs t xl = match t with
    Noeud(_,c,tl)-> match c with
      (x,y) -> aux_abs_list tl (x::xl)
    and aux_abs_list tl xl = match tl with
      []->xl
      |t::q-> aux_abs_list q (aux_abs t xl)
    in aux_abs t []

  (**renvoie la liste des ordonnées d'un arbre*)
  let getordinnates t = let rec aux_ord t yl = match t with
    Noeud(_,c,tl)-> match c with
      (x,y) -> aux_ord_list tl (y::yl)
    and aux_ord_list tl yl = match tl with
      []->yl
      |t::q-> aux_ord_list q (aux_ord t yl)
    in aux_ord t []


  (**renvoie la listes des coordonnées d'un arbre*)
  let getcoordinates t = 
    let xl = getabsciss t in 
    let yl = getordinnates t in 
    List.combine xl yl

  let getcoord_treelist tl = let t = Noeud(false,(zero,zero),tl) in let l = getcoordinates t in remove_last l

  (**renvoie les coord de tous les arbres de la liste tl mais pas celles de leurs sous-abres*)
  let rec getcoord_treelist_nosubtree tl = match tl with
  []->[]
  |t::q-> match t with
  Noeud(_,c,_)-> c::(getcoord_treelist_nosubtree q)

  (** renvoie la liste des points possibles d'un arbre rectilinéaire*)
  let getpoints t = 
    let rec aux_y y xl = match xl with
      []-> []
      |t::q-> (t,y)::(aux_y y q) in
    let rec aux xl yl = match yl with
      [] -> []
      |t::q -> (aux_y t xl)@(aux xl q) in
    let xl = getabsciss t in 
    let yl = getordinnates t in
    uniq (aux xl yl)


  (**renvoie vrai si c1 et c2 ont des abscisses ou des ordonnées voisines dans la liste cl *)
  let rec voisin c1 c2 cl mode = let rec aux_x x1 x2 cl = match cl with
  []-> true
  |t::q -> match t with
    (x,y) -> if (x1<x && x<x2 ) || (x2<x && x<x1) then false else aux_x x1 x2 q
  in let rec aux_y y1 y2 cl = match cl with
  []-> true
  |t::q -> match t with
    (x,y) -> if (y1<y && y<y2 ) || (y2<y && y<y1) then false else aux_y y1 y2 q in
  match mode with
  "x"-> begin match c1 with 
    (x1,y1) -> begin match c2 with
      (x2,y2) -> aux_x x1 x2 cl end
    end
  |"y" -> begin match c1 with 
    (x1,y1) -> begin match c2 with
      (x2,y2) -> aux_y y1 y2 cl end
    end
  |_->failwith"mauvais mode"

  (**renvoie la liste des arbres de cl voisins de c dans cl *)
  let rec voisins c cl clbis= match c with
  (x,y) -> match cl with
    []->[]
    |t::q-> match t with
      (x2,y2) -> if x == x2 then 
        if voisin c t clbis "y" then Noeud(false,t,voisins t q clbis)::(voisins c q clbis)
        else voisins c q clbis
      else if y == y2 then 
        if voisin c t clbis "x" then Noeud(false,t,voisins t q clbis)::(voisins c q clbis)
        else voisins c q clbis
      else voisins c q clbis

  (** renvoie l'arbre t dont tous les sous-arbes dont les coordonnées sont dans cl ont leur bool à true, et tous les autre ont leur bool à false *)
  let rec change_bool t cl = match t with
  Noeud(_,c,tl) -> if isin c cl then Noeud(true,c,change_bool_list tl cl) else Noeud(false,c,change_bool_list tl cl)
  and change_bool_list tl cl = match tl with
  []->[]
  |t::q -> (change_bool t cl)::(change_bool_list q cl)

    (*supprime le membre égal à p de la liste*)
  let rec deletelist p l=match l with 
  |[]->[]
  |r::q->if r=p then q else r::(deletelist p q)

  (** renvoie un graphe contenant tous les points de cl, chacun reliés à leur plus proche voisin vertical et horizontal*)
  let graphe_complet_sans_bool cl coord= 
  match coord with
  []->failwith"pas de point de base"
  |t::q-> let cl_nt = deletelist t cl in
  Noeud(false,t,voisins t cl_nt cl)

  (** renvoie un graphe complété à partir de t, ie un graphe avec tous les points supplémentaires utiles selon la distance de Manhattan
  et dont tous les sous-arbres sont reliés à leur plus proches voisins vertical et horizontal *)
  let graphe_complet t = 
    let points = getpoints t in 
      let coord = getcoordinates t in 
        change_bool (graphe_complet_sans_bool points coord) coord

  
  (**renvoie true si st est un sous-arbre de t (au sens large)*)
  let rec is_subtree st t = match t with
  Noeud(_,_,tl)-> if isin st tl then true else is_subtree_in_list st tl

  (**renvoie true si st est un sous-arbre d'un des arbres de tl*)
  and is_subtree_in_list st tl = match tl with
  []->false
  |t::q -> is_subtree st t || is_subtree_in_list st q

  

  let findcycle t = let rec aux t v = let coord_v = getcoord_treelist_nosubtree v in match t with
  Noeud(_,c,tl) -> 
  (* let _ = Printf.printf "courant noeud : "; dump_coord c ; Printf.printf" | v : "; dump_coord_tree_list v;if tl = [] then Printf.printf "nsa\n%!" else Printf.printf "\n%!"; in *)
  if (isin c coord_v) then  
  (* let _ = Printf.printf"Fin : "; dump_coord c; Printf.printf "est dans v = "; dump coord_v in  *)
  true,(t::v) else aux_list tl (t::v)
  and aux_list tl v = match tl with
  []-> false,v
  |t::q -> let result = aux t v in let result2 = aux_list q (snd result) in ( fst result || fst (result2 ) ), (snd result2)
  in fst(aux t [])


  let rec par_p t = match t with
  Noeud(_,c,tl)-> let _= dump_coord c in 1 + par_p_list tl
  and par_p_list tl = match tl with
  []->0
  |t::q->par_p t + par_p_list q

  let rec create_tree cl = match cl with
  []->failwith"liste vide"
  |t::[]-> Noeud(true,t,[])
  |t::q->Noeud(true,t,[create_tree q])

  let rec print_tree t = match t with
Noeud(b,c,tl)->Printf.printf "Noeud(%b,(" b ;dump_coord c; Printf.printf ",[" ; print_tree_list tl;Printf.printf "])"
and print_tree_list tl = match tl with
[]->Printf.printf ""
|t::q-> Printf.printf ""; print_tree t;if q != [] then Printf.printf ";" else (); print_tree_list q; Printf.printf "\n"

  (*retourne la base de l'arbre, ie les points avec un bool=true*)
  let getbase t=let rec auxbase t l=match t with
  |Noeud(b,c,tl)->if(b=true) then auxbaseb tl (c::l) else auxbaseb tl l
  and auxbaseb tl l=match tl with
  |[]->uniq l
  |p::q->auxbaseb q (auxbase p l) in auxbase t [] 
  let getbranches t=let rec auxbranches t l=match t with
  |Noeud(b,c,tl)->uniq(auxbranchesb c tl l) 
  and auxbranchesb c tl l=match tl with
  |[]->l
  |p::q->match p with |Noeud(a,d,ts)->uniq (auxbranches p ((c,d)::l))@(auxbranchesb c q ((c,d)::l)) in auxbranches t []

      (**renvoie la liste des points de l'arbre*)
    let gettreepoints t=let rec auxtreepoints t l=match t with
    |Noeud(b,c,tl)->auxtreepointsbis tl (uniq (c::l)) 
    and auxtreepointsbis tl l=match tl with 
    |[]->l
    |p::q->uniq ((auxtreepoints p l)@(auxtreepointsbis q l)) in auxtreepoints t []
  
    let is_connexe t base =  let tl = getcoordinates t in let rec aux l1 l2 = match l1 with
  []->  []
  |t::q-> if isin t l2 then aux q l2 else t::(aux q l2)
  in (aux base tl) = []
  
  let rec is_tree_useful t = match t with
  |Noeud(b,_,tl) -> if b then true else is_tree_list_useful tl
  and is_tree_list_useful tl = match tl with
  []->false
  |t::q -> (is_tree_useful t) || (is_tree_list_useful q)

  let rec del_useless_branches t = match t with
  Noeud(b,c,tl) -> if not (is_tree_list_useful tl) then Noeud(b,c,[]) else Noeud(b,c,del_useless_branches_list tl)
  and del_useless_branches_list tl = match tl with
  []->[]
  |t::q-> if not (is_tree_useful t) then del_useless_branches_list q else (del_useless_branches t)::(del_useless_branches_list q) 

  let sort_list l = List.sort (fun c1 c2 -> if c1 = c2 then 0 else if c1 < c2 then -1 else 1) l
end

