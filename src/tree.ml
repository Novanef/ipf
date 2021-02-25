module type Coord =
sig
  type dot
  val zero : dot
  val (++) : dot -> dot -> dot
  val (--) : dot -> dot -> dot
  val ( *** ) : dot -> dot -> dot
  val d_abs : dot->dot
  val distance : dot*dot -> dot*dot -> dot
  val dump_coord : dot*dot -> unit
end

(** module utilisé pour le cas rectilinéaire *)
module Rectilinear_Coord : Coord with type dot = int =
struct
  type dot = int
  type coord = dot * dot

  (**zéro pour les points rectilinéaires  *)
  let zero = 0

  (**implémentation d'opérations élémentaires pour les coordonnées rectilinéaires *)
  let (++) d1 d2 = d1 + d2
  let (--) d1 d2 = d1 - d2
  let ( *** ) d1 d2 = d1 * d2
  let d_abs d = abs d

  (**
  @requires deux coordonées c1 et c2
  @ensures retourne la distance de Manhattan entre c1 et c2 
  @raises *)
  let distance c1 c2 = match c1 with
  |(x1,y1) -> match c2 with
    |(x2,y2) -> (abs (x1-x2)) + (abs (y1 -y2))

  (**
  @requires une coordonée c
  @ensures affiche le contenu de c dans la sortie standard
  @raises *)
  let dump_coord c = match c with
  (x,y) -> Printf.printf "(%d,%d) " x y
end

(** module utilisé pour le cas euclidien *)
module Euclidian_Coord : Coord with type dot = float =
struct
  type dot = float
  type coord = dot * dot

  (** zero pour les points eulidiens *)
  let zero = 0.0

  (** implémentation d'opérations élémentaires pour les coordonées euclidiennes *)
  let (++) d1 d2 = d1 +. d2
  let (--) d1 d2 = d1 -. d2
  let ( *** ) d1 d2 = d1 *. d2
  let d_abs d = abs_float d

    (**
  @requires deux coordonnées c1 et c2
  @ensures retourne la distance usuelle entre c1 et c2
  @raises *)
  let distance c1 c2 = match c1 with
    |(x1,y1) -> match c2 with
    |(x2,y2) -> ((d_abs (x1--x2)**2.) ++ (d_abs (y1 --y2)**2.))**0.5

    (**
  @requires une coordonnée c
  @ensures affiche le contenu de c dans la sortie standard
  @raises *)
  let dump_coord c = match c with
    (x,y) -> Printf.printf "(%f,%f) " x y
end

module TreeMod(X : Coord) =
struct

  type dot = X.dot
  type coord = dot * dot

  type tree = Node of bool * coord * tree list

  let (++) = X.(++)

  let (--) = X.(--)

  let ( *** ) = X.( *** )

  let zero = X.zero

  let distance = X.distance

  let dump_coord = X.dump_coord

  (**
  @requires une liste de coordonnées l
  @ensures affiche le contenu des coordonnées de l dans la sortie standard
  @raises 
  *)
  let dump l =
    let _ = Printf.printf "{ " in
    let _ = List.iter dump_coord l in
    Printf.printf "}\n%!"

  (**
  @requires un arbre t
  @ensures affiche la coordonnée de la racine de t dans la sortie standard
  @raises 
  *)
  let dump_coord_tree t = match t with
  Node(_,c,_) -> dump_coord c
  
  (**
  @requires une liste d'arbres tl
  @ensures affiche les coordonnées des racines des arbres de tl dans la sortie standard
  @raises 
  *)
  let dump_coord_tree_list tl = Printf.printf "{ "; List.iter dump_coord_tree tl; Printf.printf " }"

  (**
  @requires une liste l et un élément e du type de ceux de l
  @ensures retourne true si e est dans l, false sinon
  @raises 
  *)  
  let rec isin e l = match l with
  []->false
  |t::q -> if t = e then true else isin e q

  (**
  @requires une liste l dont les éléments peuvent être comparés par <
  @ensures retourne la liste l trié dans l'ordre croissant sans doublon
  @raises 
  *)
  let rec uniq l = List.sort_uniq (fun x y -> if x=y then 0 else if x<y then -1 else 1) l

  (**
  @requires une liste l
  @ensures retourne l sans son dernier élément, ou l si l est vide
  @raises 
  *)
  let rec remove_last l = match l with
  []->[]
  |t::q -> if q = [] then [] else t::(remove_last q)

  (**
  @requires une arbre t et une coordonnée c
  @ensures retourne la distance entre c et la racine de t
  @raises 
  *)  let distance_to_tree c t = match t with
    Node(_,c2,tl) -> distance c c2
  
    (**
  @requires une liste d'arbres tl et une coordonnée c
  @ensures retourne la somme des distances entre c et les racines des arbres de tl
  @raises 
  *)let rec distance_tree_list c tl = match tl with
  []->zero
  |t::q -> (distance_to_tree c t) ++ (distance_tree_list c q)

  (**
  @requires un arbre t
  @ensures retourne la somme des poids des arêtes de t
  @raises 
  *)  let rec weight t = match t with
    Node(_,c,tl) -> distance_tree_list c tl ++ weight_list tl
  (**renvoie la somme des poids des arrêtes des arbres de la liste tl, marche pas si un élément de t est cyclique*)
  and weight_list tl = match tl with
  []->zero
  |t::q -> weight t ++ weight_list q

  (**
  @requires un arbre t
  @ensures retourne la liste des abscisses des coordonées de t et de ses sous-arbres (au sens large)
  @raises 
  *)  let getabsciss t = let rec aux_abs t xl = match t with
    Node(_,c,tl)-> match c with
      (x,y) -> aux_abs_list tl (x::xl)
    and aux_abs_list tl xl = match tl with
      []->xl
      |t::q-> aux_abs_list q (aux_abs t xl)
    in aux_abs t []

  (**
  @requires un arbre t
  @ensures retourne la liste des ordonnées des coordonnées de t et de ses sous-arbres (au sens large)
  @raises 
  *)  
  let getordinates t = let rec aux_ord t yl = match t with
    Node(_,c,tl)-> match c with
      (x,y) -> aux_ord_list tl (y::yl)
    and aux_ord_list tl yl = match tl with
      []->yl
      |t::q-> aux_ord_list q (aux_ord t yl)
    in aux_ord t []


  (**
  @requires un arbre t
  @ensures retourne la liste des coordonées de t et des ses sous-arbres (au sens large)
  @raises 
  *)
  let getcoordinates t = 
    let xl = getabsciss t in 
    let yl = getordinates t in 
    uniq (List.combine xl yl)

  (**
  @requires une liste d'arbres tl
  @ensures retourne la liste des coordonnées des racines des arbres de tl
  @raises 
  *)
  let rec getcoord_treelist_nosubtree tl = match tl with
  []->[]
  |t::q-> match t with
  Node(_,c,_)-> c::(getcoord_treelist_nosubtree q)

    (**
  @requires un arbre t
  @ensures retourne la liste des coordonnées dont l'abscisse et l'ordonnée appartiennent chacune a au moins une des coordonnées présentes dans t (mais pas forcément la même)
  @raises 
  *)
  let getpoints t = 
    let rec aux_y y xl = match xl with
      []-> []
      |t::q-> (t,y)::(aux_y y q) in
    let rec aux xl yl = match yl with
      [] -> []
      |t::q -> (aux_y t xl)@(aux xl q) in
    let xl = getabsciss t in 
    let yl = getordinates t in
    uniq (aux xl yl)

  (**
  @requires un arbre t
  @ensures affiche le contenu de t dans la sortie standard
  @raises 
  *)
  let rec dump_tree t = match t with
  Node(b,c,tl)->Printf.printf "Node(%b," b ;dump_coord c; Printf.printf ",[" ; dump_tree_list tl;Printf.printf "])"
  and dump_tree_list tl = match tl with
  []->Printf.printf ""
  |t::q-> Printf.printf ""; dump_tree t;if q != [] then Printf.printf ";" else (); dump_tree_list q; Printf.printf "\n"


  (**
  @requires deux coordonnées c1 et c2, une liste de coordonnées cl et un string mode
  @ensures retourne true si c1 et c2 ont leurs abscisses (resp. ordonnées) égales et qu'aucune ordonnée (resp. abscisse) de cl n'est incluse entre celles de c1 et c2
  @raises failures "mauvais mode" si mode est différent de "x" ou "x"
  *)
  let rec neighbor c1 c2 cl mode = let rec aux_x x1 x2 cl = match cl with
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

  (**
  @requires une coordonnée c et deux liste de coordonnées identiques cl et clbis
  @ensures retourne la liste des coordonnées c' voisins de c tel que défini dans neighbor
  @raises 
  *)
  let rec neighbors c cl clbis= match c with
  (x,y) -> match cl with
    []->[]
    |t::q-> match t with
      (x2,y2) -> if x = x2 then 
        if neighbor c t clbis "y" then Node(false,t,neighbors t q clbis)::(neighbors c q clbis)
        else neighbors c q clbis
      else if y = y2 then
        if neighbor c t clbis "x" then Node(false,t,neighbors t q clbis)::(neighbors c q clbis)
        else neighbors c q clbis
      else neighbors c q clbis

  (**
  @requires un arbre t et une liste de coordonnées cl
  @ensures retourne l'arbre t avec tous ses sous-arbres (t inclus) dont la coordonnée de la racine est incluse dans cl ont leur booléen à true, et dont tous ses autres sous-arbres ont leur booléen à false
  @raises 
  *)
  let rec change_bool t cl = match t with
  Node(_,c,tl) -> if isin c cl then Node(true,c,change_bool_list tl cl) else Node(false,c,change_bool_list tl cl)
  and change_bool_list tl cl = match tl with
  []->[]
  |t::q -> (change_bool t cl)::(change_bool_list q cl)

  (**
  @requires une liste l et un élément p du type de ceux de l
  @ensures retourne l dont p a été supprimé s'il était dedans
  @raises 
  *)
  let rec deletelist p l=match l with 
  |[]->[]
  |r::q->if r=p then q else r::(deletelist p q)

   (**
  @requires une liste de coordonées cl
  @ensures retourne un arbre contenant toutes les coordonnées de cl et dont la réprésentation graphique forme une grille
  @raises 
  *) let complete_graph_no_bool cl= 
  match cl with
  []->failwith"pas de point de base"
  |t::q-> Node(false,t,neighbors t q cl)

  (**
  @requires une liste l dont les éléments sont comparables par <
  @ensures retourne l trié par ordre croissant
  @raises 
  *)
  let sort_list l = List.sort (fun c1 c2 -> if c1 = c2 then 0 else if c1 < c2 then -1 else 1) l



  (**
  @requires un arbre t
  @ensures retourne un arbre contenant toutes les coordonnées présentes dans t, dont la représentation forme une grille et dont tous les arbres dont la coordonnée de la racine correspondait à un arbre avec un booléen true dans t ont leur booléen à true, à false sinon
  @raises 
  *)
  let complete_graph t = 
  let coord = getcoordinates t in
  let points =  sort_list(getpoints t) in
  change_bool (complete_graph_no_bool points) coord

  (**
  @requires deux arbres st et t
  @ensures retourne true si st est un sous-arbre (au sens large) de t (t non inclus)
  @raises 
  *)
  let rec is_subtree st t = match t with
  Node(_,_,tl)-> if isin st tl then true else is_subtree_in_list st tl
  and is_subtree_in_list st tl = match tl with
  []->false
  |t::q -> is_subtree st t || is_subtree_in_list st q

  (**
  @requires un arbre t
  @ensures retourne true si t contient un sous-arbre auquel il est possible d'accéder depuis deux sous-arbres différents de t
  @raises 
  *)
  let findcycle t = let rec aux t v = let coord_v = getcoord_treelist_nosubtree v in match t with
  Node(_,c,tl) -> 
  if (isin c coord_v) then  
  true,(t::v) else aux_list tl (t::v)
  and aux_list tl v = match tl with
  []-> false,v
  |t::q -> let result = aux t v in let result2 = aux_list q (snd result) in ( fst result || fst (result2 ) ), (snd result2)
  in fst(aux t [])

  (**
  @requires une liste de coordonnées cl non vide
  @ensures retourne un arbre contenant toutes les coordonnées de t
  @raises failure "liste vide" si cl est vide
  *)
  let rec create_tree cl = match cl with
  []->failwith"liste vide"
  |t::[]-> Node(true,t,[])
  |t::q->Node(true,t,[create_tree q])

  (**
  @requires un arbre t
  @ensures retourne la liste des coordonnées des racines des sous-arbres de t (au sens large, t inclus) dont le booléen est true
  @raises 
  *)
  let getbase t=let rec auxbase t l=match t with
  |Node(b,c,tl)->if(b=true) then auxbaseb tl (c::l) else auxbaseb tl l
  and auxbaseb tl l=match tl with
  |[]->uniq l
  |p::q->auxbaseb q (auxbase p l) in auxbase t [] 
  let getbranches t=let rec auxbranches t l=match t with
  |Node(b,c,tl)->uniq(auxbranchesb c tl l) 
  and auxbranchesb c tl l=match tl with
  |[]->l
  |p::q->match p with |Node(a,d,ts)->uniq (auxbranches p ((c,d)::l))@(auxbranchesb c q ((c,d)::l)) in auxbranches t []

  (**
  @requires un arbre t est une liste de coordonnées base
  @ensures retourne true si toutes les coordonnées de base se retrouvent dans t
  @raises 
  *)
  let is_connected t base =  let tl = uniq (getcoordinates t) in let rec aux l1 l2 = match l1 with
  []->  []
  |t::q-> if isin t l2 then aux q l2 else t::(aux q l2)
  in (aux base tl) = []
  
  (**
  @requires un arbre t
  @ensures retourne true si au moins un des sous-arbres de t (au sens large, t inclus) a son booléen à true
  @raises 
  *)
  let rec is_tree_useful t = match t with
  |Node(b,_,tl) -> if b then true else is_tree_list_useful tl
  and is_tree_list_useful tl = match tl with
  []->false
  |t::q -> (is_tree_useful t) || (is_tree_list_useful q)

  (**
  @requires un arbre t
  @ensures retorune t dont toutes les branches inutiles au sens de is_tree_useful ont été supprimées
  @raises 
  *)
  let rec del_useless_branches t = 
  let rec aux t = match t with
  Node(b,c,tl) -> if not (is_tree_list_useful tl) then Node(b,c,[]) else Node(b,c,aux_list tl)
  and aux_list tl = match tl with
  []->[]
  |t::q-> if not (is_tree_useful t) then aux_list q else (aux t)::(aux_list q) 
  in match t with
  Node(b,c,tl) -> if (List.length tl = 1) && (not b) then del_useless_branches (List.hd tl) else aux t


  (**
  @requires un arbre t
  @ensures retourne la liste de tous les arbres de t (t inclus)
  @raises 
  *)
  let rec get_all_trees t = match t with
  Node(_,_,tl) -> t::(get_all_trees_list tl)
  and get_all_trees_list tl = match tl with
  []->[]
  |t::q-> (get_all_trees t)@(get_all_trees_list q)

  (**
  @requires un arbre t
  @ensures retourne le nombre de sous-arbres (directs, t non inclus) de t
  @raises 
  *)
  let nb_subrees t = match t with
  Node(_,_,tl) -> List.length tl

  (**
  @requires trois coordonnées a b et c
  @ensures retourne true si elles sont alignées, false sinon
  @raises 
  *)
  let are_aligned a b c = match a with 
  (xa,ya) -> match b with (xb,yb) -> match c with (xc,yc) ->
  ((xa -- xc) *** (ya -- yb)) -- ((ya -- yc) *** (xa -- xb)) = zero

  (**
  @requires une liste de coordonnées l
  @ensures retourne true si toutes les coordonnées de l sont alignées, false sinon
  @raises 
  *)
  let rec is_list_a_line l = match l with
  []->true
  |a::[] -> true
  |a::b::[]->true
  |a::b::c::q -> are_aligned a b c && is_list_a_line q
  
  (**
  @requires une liste l non vide
  @ensures retourne un élément de l au hasard
  @raises failure "getrandom : liste vide" si l est vide
  *)  
  let getrandom l=if(List.length l)>0 then let rec auxrand l x=match l with 
  |[]->failwith"getrandom : liste vide"
  |p::q->if x=0 then p else auxrand q (x-1) 
  in auxrand l (Random.int (List.length l)) else failwith"getrandom : liste vide"

  let rec get_root t c_st = match t with
    Node(b,c,tl) -> if c = c_st || isin c_st (getcoord_treelist_nosubtree tl) then t else get_root_list tl c_st
    and get_root_list tl c_st = match tl with
    []->failwith"pas trouvé"
    |t::q-> if isin c_st (getcoordinates t) then get_root t c_st else get_root_list q c_st
end


