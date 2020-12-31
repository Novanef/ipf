module type Coord =
sig
  type dot
  val zero : dot
  val (++) : dot -> dot -> dot
  val distance : dot*dot -> dot*dot -> dot
  val dump_coord : dot*dot -> unit
end

module Coord_R : Coord with type dot = int =
struct
  type dot = int
  type coord = dot * dot

  let zero = 0

  let (++) d1 d2 = d1 + d2

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
let distance coord1 coord2 = 1.0 (**TODO *)

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

  let dump_coord_tree_list tl = Printf.printf "{ "; List.iter dump_coord_tree tl; Printf.printf " }\n%!"

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

  let rec change_bool t cl = match t with
  Noeud(_,c,tl) -> if isin c cl then Noeud(true,c,change_bool_list tl cl) else Noeud(false,c,change_bool_list tl cl)
  and change_bool_list tl cl = match tl with
  []->[]
  |t::q -> (change_bool t cl)::(change_bool_list q cl)

  (** renvoie le graphe bien mec faudra changer ce comm *)
  let graphe_complet_sans_bool cl = match cl with
    []->failwith"listevide"
    |t::q -> Noeud(false,t,voisins t q cl)

  let graphe_complet t = 
    let points = getpoints t in 
      let coord = getcoordinates t in 
        change_bool (graphe_complet_sans_bool points) coord

  (* let findCycle t = let rec aux t v = match t with
    Noeud(_,c,tl) ->  if isin c v then true else aux_list tl (c::v)
    and aux_list tl v = match tl with
      []->false
      |t::q -> match t with
        Noeud(_,c,tl2) -> aux t v || aux_list tl2 (c::v)
    in aux t [] *)
  
  (**renvoie true si st est un sous-arbre de t (au sens large)*)
  let rec is_subtree st t = match t with
  Noeud(_,_,tl)-> if isin st tl then true else is_subtree_in_list st tl

  (**renvoie true si st est un sous-arbre d'un des arbres de tl*)
  and is_subtree_in_list st tl = match tl with
  []->false
  |t::q -> is_subtree st t || is_subtree_in_list st q

  let findcycle t = let rec aux t v = if (isin t v) then (true,t::v) else match t with
  Noeud(_,c,tl) -> 
  (* let _ = Printf.printf "courant noeud : "; dump_coord c ; Printf.printf" | v : "; dump_coord_tree_list v in *)
   aux_list tl (t::v)
  and aux_list tl v = match tl with
  []->false,v
  |t::q -> let result = aux t v in ( fst result || fst ( aux_list q (snd result) ) ),(t::v)
  in fst(aux t [])


  let rec par_p t = match t with
  Noeud(_,c,tl)-> let _= dump_coord c in 1 + par_p_list tl
  and par_p_list tl = match tl with
  []->0
  |t::q->par_p t + par_p_list q
end

