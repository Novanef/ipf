module type Coord =
sig
  type dot
  val zero : dot
  val (++) : dot -> dot -> dot
  val distance : dot*dot -> dot*dot -> dot
end

module Coord_R : Coord with type dot = int =
struct
  type dot = int
  type coord = dot * dot

  let zero = 0

  let (++) d1 d2 = d1 + d2

  (*retourne la distance de Manhattan entre c1 et c2*)
  let distance c1 c2 = match c1 with
  |(x1,y1) -> match c2 with
    |(x2,y2) -> (abs (x1-x2)) + (abs (y1 -y2))
end

module Coord_E : Coord with type dot = float =
struct
type dot = float
type coord = dot * dot

let zero = 0.0

let (++) d1 d2 = d1 +. d2
let distance coord1 coord2 = 1.0 (*TODO *)
end

module FoncteurTree(X : Coord) =
struct

  type dot = X.dot
  type coord = dot * dot

  type tree = Noeud of bool * coord * tree list

  let (++) = X.(++)

  let zero = X.zero

  let distance = X.distance


  let rec mem l i=match l with
  |[]->true
  |p::q->if i==p then false else mem q i

  let rec uniq l = match l with
    [] -> []
    |t::q -> if mem q t then uniq q else t::(uniq q)

  (*renvoie la distance entre la coord c et l'arbre t *)
  let distance_to_tree c t = match t with
    Noeud(b,c2,tl) -> distance c c2
  
  (* renvoie la somme des distances entre la coord c et tous les abres de la liste tl*)
  let rec distance_tree_list c tl = match tl with
  []->zero
  |t::q -> (distance_to_tree c t) ++ (distance_tree_list c q)

  (* renvoie la somme des poids des arrêtes de t*)
  let rec weight t = match t with
    Noeud(b,c,tl) -> distance_tree_list c tl ++ weight_list tl
  (*renvoie la somme des poids des arrêtes des arbres de la liste tl *)
  and weight_list tl = match tl with
  []->zero
  |t::q -> weight t ++ weight_list q

  (*renvoie la liste des abscisses d'un arbre*)
  let getabsciss t = let rec aux_abs t xl = match t with
    Noeud(b,c,tl)-> match c with
      (x,y) -> aux_abs_list tl (x::xl)
    and aux_abs_list tl xl = match tl with
      []->xl
      |t::q-> aux_abs_list q (aux_abs t xl)
    in aux_abs t []

    (*renvoie la liste des ordonnées d'un arbre*)
  let getordinnates t = let rec aux_ord t yl = match t with
    Noeud(b,c,tl)-> match c with
      (x,y) -> aux_ord_list tl (y::yl)
    and aux_ord_list tl yl = match tl with
      []->yl
      |t::q-> aux_ord_list q (aux_ord t yl)
    in aux_ord t []

  (*renvoie les listes des abscisses et ordonnées d'un arbre*)
  let getcoordinates t = 
    let xl = getabsciss t in 
    let yl = getordinnates t in 
    xl,yl

  (*renvoie la liste des points possibles d'un arbre rectilinéaire*)
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


  (*renvoie vrai si c1 et c2 ont des abscisses ou des ordonnées voisines dans la liste cl *)
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

  (*renvoie la liste des arbres de cl voisins de c dans cl *)
  let rec voisins c cl clbis= match c with
  (x,y) -> match cl with
    []->[]
    |t::q-> match t with
      (x2,y2) -> if x == x2 then 
        if voisin c t clbis "x" then Noeud(false,t,voisins t q clbis)::(voisins c q clbis)
        else voisins c q clbis
      else if y == y2 then 
        if voisin c t clbis "y" then Noeud(false,t,voisins t q clbis)::(voisins c q clbis)
        else voisins c q clbis
      else voisins c q clbis

  let graphe_complet cl = match cl with
    []->failwith"listevide"
    |t::q -> Noeud(false,t,voisins t q cl)

  let findCycle t = let rec aux t v = match t with
    Noeud(_,c,tl) ->  match v with
    t::q -> if mem q c then true else aux_list tl (c::v)
    |[] -> aux_list tl (c::v)
    and aux_list tl v = match tl with
      []->false
      |t::q -> match t with
        Noeud(_,c,tl2) -> aux t v || aux_list q (c::v)
    in aux t []
end

