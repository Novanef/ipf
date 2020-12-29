module type Coord =
sig
  type dot
  val zero : dot
  type coord
  val (++) : dot -> dot -> dot
  val distance : coord -> coord -> dot
end

module Coord_R : Coord =
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

module Coord_E : Coord =
struct
type dot = float
type coord = dot * dot

let zero = 0.0

let (++) d1 d2 = d1 +. d2
let distance coord1 coord2 = 1.0
end

module FoncteurTree(X : Coord) =
struct
  type tree =
  Empty 
  | Noeud of bool * X.coord * tree list

  let (++) = X.(++)

  let zero = X.zero

  let distance = X.distance

  (*renvoie la distance entre la coord c et l'arbre t *)
  let distance_to_tree c t = match t with
    Empty->zero
    |Noeud(b,c2,tl) -> distance c c2
  
  (* renvoie la somme des distances entre la coord c et tous les abres de la liste tl*)
  let rec distance_tree_list c tl = match tl with
  []->zero
  |t::q -> (distance_to_tree c t) ++ (distance_tree_list c q)

  (* renvoie la somme des poids des arrêtes de t*)
  let rec weight t = match t with
    Empty -> zero
    |Noeud(b,c,tl) -> distance_tree_list c tl ++ weight_list tl
  (*renvoie la somme des poids des arrêtes des arbres de la liste tl *)
  and weight_list tl = match tl with
  []->zero
  |t::q -> weight t ++ weight_list q

end