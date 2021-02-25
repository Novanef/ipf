(*Euclidien*)
open Tree
module Euclidian_Tree = TreeMod(Euclidian_Coord)
open Euclidian_Tree
    
    
  (**
  @requires un arbre t
  @ensures retourne la liste des coordonnées des racines des sous-arbres (au sens large, t inclus) de t qui ont pour leur booléen false
  @raises 
  *)
  let getrelay t = 
    let rec auxrelay t l = match t with
      |Node(b,c,tl)-> if b then 
        auxrelayb tl l
      else 
        auxrelayb tl (c::l)
    and auxrelayb tl l = match tl with
      |[]->uniq l
      |p::q->auxrelayb q (auxrelay p l) 
  in auxrelay t [] 
    
    
  (**
  @requires un arbre t et deux coordonnées c1 et c2
  @ensures retourne l'arbre t auquel on ajouté Node(true,c2,[]) comme sous-arbre au sous-arbre (au sens large, t inclus) de t dont la racine a pour coordonnée c1 s'il un tel sous-arbre existe, sinon t
  @raises 
  *)
  let rec putpoint t c1 c2 = match t with 
    |Node(b,c,tl)->if c = c1 then
      Node(b,c,(Node(true,c2,[])::tl)) 
    else
      Node(b,c,(auxputpoint tl c1 c2))
  and auxputpoint tl c1 c2 = match tl with
    |[]->[]
    |t::q->(putpoint t c1 c2)::(auxputpoint q c1 c2)
  
  (**
  @requires une liste de coordonnées entière l
  @ensures retourne la liste l dont toutes les coordonnées entières ont été converties en coordonnées flottantes
  @raises 
  *)
  let rec int_coord_list_to_float_coord_list l = match l with
    []->[]
    |(x,y)::q -> (float_of_int x,float_of_int y)::(int_coord_list_to_float_coord_list q)
      
  (**
  @requires une liste de coordonnées cl
  @ensures retourne un arbre dont la liste des coordonnées des racines de ses sous-arbres (au sens large, lui-même inclus) est cl à l'ordre prêt
  @raises 
  *)  
  let create_random_tree cl = 
  let rec auxcreatetree cl t acc =
    if cl = [] then
    t
    else 
      let c = getrandom cl in 
      if acc = [] then 
        auxcreatetree (deletelist c cl) (Node(true,c,[])) (c::acc) 
      else 
        let k = getrandom acc in 
        auxcreatetree (deletelist c cl) (putpoint t k c) (c::acc) 
  in auxcreatetree cl (Node(false,(0.0,0.0),[])) []
    
  (**
  @requires une liste l non vide de couples d'objets comparables avec <
  @ensures retourne le couple composé du minimum des premiers des couples de l et du minimum des seconds des couples de l
  @raises failure "getmin : liste vide" si l est vide
  *)  
  let getmin l =
    let rec aux l (x_min,y_min) = match l with
      []->(x_min,y_min)
      |(x,y)::q -> aux q (min x x_min,min y y_min)
    in
    if l = [] then
      failwith"getmin : liste vide"
    else 
      aux (List.tl l) (List.hd l)
  
  (**
  @requires une liste l non vide de couples d'objets comparables avec <
  @ensures retourne le couple composé du maximum des premiers des couples de l et du maximum des seconds des couples de l
  @raises failure "getmax : liste vide" si l est vide
  *)  
  let getmax l =
    let rec aux l (x_max,y_max) = match l with
      []->(x_max,y_max)
      |(x,y)::q -> aux q (max x x_max,max y y_max)
    in
    if l = [] then
      failwith"getmax : liste vide"
    else 
      aux (List.tl l) (List.hd l)
  
  (**
  @requires quatre paire de flottants p, t1, t2 et t3
  @ensures retourne true si p est à l'intérieur du triangle formé par t1,t2 et t3, false sinon
  @raises 
  *)  
  let is_in_triangle p t1 t2 t3 =
    let aux (a,b) (c,d) (e,f) = ( (a -. e) *. (d -. f) ) -. ( ( c -. e ) *. ( b -. f ) )
    in 
    let aux1,aux2,aux3 = (aux p t1 t2),(aux p t2 t3),(aux p t3 t1) in
    not ( (aux1 < 0. || aux2 < 0. || aux3 < 0. ) && (aux1 > 0. || aux2 > 0. || aux3 > 0.) )

  (**
  @requires un arbre t
  @ensures retourne le nombre de sous-arbres (directs, t non inclus) de t ayant aux-même au moins un sous-arbre (direct,eux-même non inclus)
  @raises 
  *)      
  let nb_subtrees_deep t =
    let rec aux tl = match tl with
      []->0
      |t::q-> if nb_subrees t > 0 then 1 + aux q else aux q
    in match t with
      Node(_,_,tl) -> aux tl

  (**
  @requires une liste l
  @ensures retourne la liste de tous les couples possibles composés de deux éléments de l sans prendre en compte l'ordre
  @raises 
  *)  
  let rec all_couples l = match l with
    []->[]
    |t::[] -> []
    |t::q -> let func t x = (t,x) in (List.map (func t) q)@(all_couples q)

  (**
  @requires un arbre t
  @ensures retourne all_couple tl où tl est la liste des sous-arbres (directs, t non inclus) de t
  @raises 
  *)  
  let list_subtree_subtree t = match t with
    Node(_,_,tl) -> all_couples tl

  (**
  @requires un arbre t
  @ensures retourne la liste de tous les couples possibles composé avec en premier élément un sous-arbre (direct, t non-inclus) t' ayant au moins un sous-arbre (direct, t' non inclus) et en deuxième élément un sous-arbre (direct, t' non inclus) de t', sans prendre en compte l'ordre
  @raises 
  *)  
  let list_subtree_subsubtree t =
    let rec aux t tl = match tl with
      []->[]
      |Node(b,c,ttl)::q -> let func t x = (t,x) in (List.map (func (Node(b,c,ttl))) ttl )@(aux t q)
    in
    match t with
      Node(_,_,tl) -> aux t tl


  (**
  @requires un arbre t contenant au moins trois points
  @ensures retourne une liste de trois points correspondant à trois arbres de t dont deux sont des sous-arbres (direct, non-inclus) du troisième ou dont l'un est le sous-arbre (direct,non-inclus) d'un autre qui est le sous-arbre (direct, non-inclus) du premier
  @raises 
   *)  
  let gettriangle t =
    let rec aux tree_list = 
      let first = getrandom tree_list in match first with Node(_,f_c,_) ->
      if (nb_subrees first) > 1 || (nb_subtrees_deep first) > 0 then
        let l = (list_subtree_subtree first)@(list_subtree_subsubtree first) in
        let rest = getrandom l in match rest with
          (Node(_,c2,_),Node(_,c3,_)) -> match first with Node(_,c1,_) ->
            if are_aligned c1 c2 c3 then
              aux tree_list
            else [c1;c2;c3]
      else aux (deletelist first tree_list)
    in let tree_list = get_all_trees t in  aux tree_list

  (**
  @requires un arbre t et une liste de coordonnées cl
  @ensures retourne la liste des sous-arbres (direct,t non inclus) de t sans ceux dont la coordonnée de la racine est dans cl
  @raises 
   *)  
  let filter_subtree_list (Node(b,c,tl)) cl =
    let rec auxcheck tl cl = match tl with 
      |[]->[]
      |Node(a,d,tll)::q -> if isin d cl then 
          auxcheck q cl
        else 
          (Node(a,d,tll))::(auxcheck q cl) 
    in auxcheck tl cl 
      
  (**
  @requires une coordonnée c_st et un arbre t dont un sous-arbre (au sens large, t inclus) a pour coordonnée de racine c_st
  @ensures retourne le sous-arbre dont la coordonnée de racine est c_st
  @raises  failure "c_st n'est pas dans t" si c_st n'est pas dans t
   *)  
  let get_subtree c_st t =
    let rec aux c_st t l = match t with 
      |Node(b,c,tl)->if c = c_st then 
        Node(b,c,tl)::l 
      else 
        aux_list c_st tl l
    and aux_list c_st tl l = match tl with
      |[]->l
      |r::q->(aux c_st r l)@(aux_list c_st q l) 
    in match (aux c_st t []) with
      |[]->failwith"c_st n'est pas dans t"
      |c_st::q-> c_st
    
  (**
  @requires un arbre t
  @ensures retourne la liste des coordonnées des racines des sous-arbres (directs, t non inclus) de t
  @raises 
   *) 
  let getcoord_tree_nosubtree (Node(b,c,tl)) = getcoord_treelist_nosubtree tl
      
  (**
  @requires une liste de coordonées tri d'au moins trois éléments
  @ensures retourne un point à l'intérieur du triangle formé par les trois premières coordonées de tri
  @raises failure "pas assez de point" si tri a moins de trois éléments
   *) 
  let get_point_in_triangle tri =
    let max,min = getmax tri, getmin tri 
    in let rec aux p1 p2 p3 = 
      let aux2 max min =
        let p = (Random.float (fst (max)-.fst (min))+.fst min,Random.float(snd(max)-.snd(min))+.snd(min)) 
        in if (is_in_triangle p p1 p2 p3) then 
          p 
        else 
          aux p1 p2 p3 
      in aux2 max min 
    in match tri with
    |t1::t2::t3::q -> aux t1 t2 t3
    |_->failwith"pas assez de point"
     
  (**
  @requires un arbre t et une liste de coordonnées l dont tous les éléments sont des coordonnées de racine de sous-arbres (au sens large, t inclus) de t
  @ensures retourne la liste des sous-arbres de t dont la racine a pour coordonnée un élément de l
  @raises failure "c_st n'est pas dans t" si une coordonnée de l n'est pas dans t
   *)
  let rec get_subtree_list t l = match l with
    []->[]
    |c::q -> (get_subtree c t)::(get_subtree_list t q)

  (**
  @requires une liste d'arbre tl et une liste de coordonées l
  @ensures retourne la liste des arbres t de tl auxquels on a applisqué filter_subtree_list t l
  @raises 
   *)
  let rec filter_tree_list tl l = match tl with 
    |[]->[]
    |p::q->match p with
      Node(b,c,tll)-> (Node(b,c, (filter_subtree_list p l) ))::(filter_tree_list q l)

  (**
  @requires un arbre t, une coordonnée p et une liste de coordonées l dont les éléments sont les coordonnées des racines de sous-arbres (au sens large, t inclus) de t
  @ensures retourne un arbre de coordonnée de racine p et dont les sous-arbres sont les sous-arbres (au sens large, t inclus) de t dont les coordonnées des racines sont les éléments de l, sans laison entre eux
  @raises failure "c_st n'est pas dans t" si une coordonnée de l n'est pas dans t
   *)    
  let insert t p l = Node(false,p,filter_tree_list (get_subtree_list t l) l)  
    
  (**ajoute un point généré aléatoirement dans le triangle formé par 3 points choisi aléatoirement parmi ceux de t
  puis supprime les arètes entre ces 3 points et les relie à p*)
  (**
  @requires un arbre t avec au moins trois éléments différents dans la liste des coordonnées des racines de ses sous-arbres (au sens large, t inclus)
  @ensures retourne t auquel on lui ajouté un sous-arbre (au sens large,t non inclus) de coordonnée de racine dans un triangle formé par les coordonnées des racines de trois sous-arbres (au sens large,t non inclus) de t. Ce nouveau sous-arbre a pour sous-arbres (au sens strict) les trois précédents qui ne sont plus réliés entre eux
  @raises failure "moins de trois points" si t a moins de trois coordonées différentes
   *)    
  let insert_relay_point t = 
    if(List.length (getcoordinates t)) < 3 then 
      failwith"moins de 3 points" 
    else let l = (gettriangle t) in
      let pt = get_point_in_triangle l in
        let rec auxadd t p l = match p with
          |Node(b,c,tl)->
            if isin c l then
              insert t pt l,true 
            else 
              Node(b,c,auxbadd tl l),false
        and auxbadd tl l = match tl with 
          |[]->[]
          |p::q->
            if snd(auxadd t p l) then 
              fst(auxadd t p l)::q 
            else 
              p::(auxbadd q l) 
      in fst(auxadd t t l)
    
    
    
  (**
  @requires un arbre t avec au moins un point relais
  @ensures retourne t avec la coordonée de la racine d'un de ses points relais déplacée dans le triangle où elle était à l'origine
  @raises 
   *)  
  let move_relay_point t = 
    let rec aux st p = match st with
      |Node(b,c,tl)->if c = p then
        Node(b,get_point_in_triangle (gettriangle t),tl) 
      else 
        Node(b,c,aux_list tl p)
    and aux_list tl p = match tl with
      |[]->[]
      |r::q->(aux r p)::(aux_list q p)  
    in aux t (getrandom (getrelay t))
    
  (**
  @requires deux liste l et s dont les éléments sont du même type
  @ensures retourne la concaténation des deux, sans doublon
  @raises 
   *)  
  let rec concat l s = match l with 
    |[]-> uniq s
    |p::q->if isin p s then concat q s else concat q (p::s)
    
  (**
  @requires un arbre t et une liste d'arbre l
  @ensures retourne t avec les arbres de l ajoutés à sa liste de sous-arbres (direct, t non inclus)
  @raises 
   *)  
  let rec merge (Node(b,c,tl)) l = Node(b,c,concat l tl)

  (**
  @requires un arbre t avec au moins un point relais
  @ensures retourne t dont un point relais tiré au hasard a été fusionné avec un de ses voisins
  @raises failure "liste vide" si t n'as pas de point relais
   *) 
  let merge_relay_point t =
    let rec aux c_p s p = match s with 
      |Node(b,c,tl)->if c = p then 
        if tl = [] then 
          get_subtree c_p t 
        else 
          let pt = get_subtree (getrandom (getcoord_tree_nosubtree s)) s 
          in (merge pt (deletelist pt tl)) 
      else Node(b,c,aux_list c tl p)
    and aux_list c_p tl p = match tl with
      |[]->[]
      |r::q->(aux c_p r p)::(aux_list c_p q p)  
    in match t with Node(_,c,_)-> aux c t (getrandom (getrelay t))
    
  (**renvoie une liste contenant un seul arbre différente de coordonnées du point p*)
  (**
  @requires une coordonnée p, une liste d'arbres tl et un arbre t
  @ensures retourne une liste vide si tl est vide ou une liste contenant un sous-arbre de t (au sens large, t inclus) dont la coordonée de la racine n'est ni p ni celle du premier élément de tl
  @raises 
   *) 
  let replace p tl t = match tl with 
    |[]->[]
    |Node(b,c,tll)::q-> let pt = getrandom((deletelist c (deletelist p (getcoordinates t)))) 
    in [get_subtree pt t]      

  (**
  @requires un arbre t et une coordonnée p_b de la base de t dont le sous-arbre correspondant n'a qu'un sous-arbre direct si c'est t ou zéro si c'est un sous-arbre (au sens large, t non inclus) de t
  @ensures retourne une coordonnée d'une racine d'un sous-arbre (au sens large,t inclus) de t qui n'a pas comme sous-arbre direct celui dont la coordonée de la racine est p_b et qui n'est pas sous-arbre direct de celui dont la coordonnée de la racine est p_b
  @raises failure "mauvais arbre ou mauvais point de base" si p_b est la coordonnée de la racine de t et si t a zéro ou plus d'un sous-arbre direct
   *)       
  let get_p_r t p_b = match t with
    Node(b,c,tl) -> if c = p_b then 
      match tl with 
        st::[] -> begin match st with Node(_,c_st,_) -> getrandom (deletelist c_st (getcoordinates st) ) end
        |_->failwith"mauvais arbre ou mauvais point de base"
    else
      match get_root t p_b with Node(_,c_r,_) ->
        getrandom  (deletelist p_b (deletelist c_r (getcoordinates t) ))

  (**
  @requires un arbre t et deux coordonées p_b et p_r, p_r contenue dans t
  @ensures retourne t dont le sous-arbre (au sens large, t inclus) dont la coordonnée de la racine est p_r a en plus de ses sous-arbres directs pré-existant Node(true,p_b,[])
  @raises failure "p_r introuvable dans t" si p_r n'est pas dans t
   *)  
  let add_p_b_to_p_r t p_b p_r = 
    let rec aux t p_b p_r = match t with
    Node(b,c,tl) -> if c = p_r then 
      Node(b,c,Node(true,p_b,[])::tl),true
    else 
      let res = aux_list tl p_b p_r 
      in Node(b,c,fst res),snd res
    and aux_list tl p_b p_r = match tl with
      []->[],false
      |t::q-> let try1 = aux t p_b p_r 
      in if snd try1 then 
        (fst try1::q),true 
      else 
        let res = aux_list q p_b p_r 
        in t::(fst res),snd res
    in let try1 = aux t p_b p_r 
    in if snd try1 then 
      fst try1 
    else 
      failwith"p_r introuvable dans t"

  (**
  @requires un arbre t et une coordonnée p_b
  @ensures retourne t auquel on a retiré le sous-arbre (au sens large,t non inclus) de t de coordonnée de racine p_b
  @raises 
   *) 
  let del_p_b t p_b =
    let rec aux_list tl p_b = match tl with
      []->[]
      |t::q-> let n_t = aux t p_b 
      in match n_t with 
        Node(b,c,tl_tl) -> if c = p_b then 
          aux_list q p_b
        else 
          t::(aux_list q p_b)
    and aux t p_b = match t with 
        Node(b,c,tl) -> Node(b,c,aux_list tl p_b)
    in aux t p_b
      
  (**
  @requires un arbre t et une coordonnée p_b de la base de t dont le sous-arbre correspondant n'a qu'un sous-arbre direct si c'est t ou zéro si c'est un sous-arbre (au sens large, t non inclus) de t
  @ensures retourne t auquel on supprimé la liaison avec p_b et rajouté une liaison vers depuis un sous-arbre (au sens large, t inclus) de t qui n'était pas relié à p_b à l'origine
  @raises failure "mauvais arbre" si p_b est la coordonnée de la racine de t et si t a zéro ou plus d'un sous-arbre direct
   *)   
  let change_edge t p_b = let p_r = get_p_r t p_b 
    in match t with
      Node(_,c,tl) -> if c = p_b then 
        match tl with
          st::[] -> add_p_b_to_p_r st p_b p_r
          |_ ->failwith"mauvais arbre"
      else 
        let del_t = del_p_b t p_b 
        in add_p_b_to_p_r del_t p_b p_r

  (**
  @requires un arbre t
  @ensures retourne tous les points de base de t qui sont à la racine ou qui n'ont pas de sous-arbre
  @raises 
   *)  
  let get_base_point_one_edge t = 
    let rec aux t = match t with
      Node(b,c,tl) -> if b && (tl = []) then 
        [c] 
      else 
        aux_list tl
    and aux_list tl = match tl with
      []->[]
      |t::q -> (aux t)@(aux_list q)
    in match t with
      Node(b,c,tl) -> if b && (List.length tl) = 1 then 
        c::(aux t) 
      else 
        aux t

  (**
  @requires un arbre t
  @ensures retourne t auquel on a appliqué change_edge avec un point aléatoire parmi get_base_point_one_edge
  @raises 
   *)  
  let newbranch t = change_edge t (getrandom (get_base_point_one_edge t))

  (**
  @requires un arbre t
  @ensures retourne t auquel in a appliqué une transformation parmi merge_relay_point t, move_relay_point t, insert_relay_point t ou newbranch t
  @raises 
  a noter que si move_relay point ou merge_relay_point est choisi mais que t n'a pas de point relais, insert_relay_point est appliqué à la place
  a noter que si get_base_point_one_edge t est vide, insert_relay_point est appliqué à la place
   *)   
  let chose_transformation t = let r = Random.int 4 in match r with
      |0->insert_relay_point t
      |1->if (List.length (getrelay t)>0) then merge_relay_point t else insert_relay_point t
      |2->if (List.length (getrelay t)>0) then move_relay_point t else insert_relay_point t
      |3-> if (List.length (get_base_point_one_edge t)>0) then newbranch t else insert_relay_point t
      |_->failwith"impossible"
      
  (**
  @requires un arbre t et un entier n
  @ensures retourne le plus petit (au sens de weight t) de n arbres acycliques reliés à toutes les coordonnées de la base générés aléatoirement
  @raises 
  *)

  open Display

  let gen_Steiner base p n = if (List.length p) < 3 then (create_random_tree p) else 
      if (is_list_a_line p) then create_tree (sort_list p) else
        let rec generatecandidat_e t n =
        if n = 0 then 
          t 
        else let g = chose_transformation t in 
          if(weight t) >= (weight g) then 
            generatecandidat_e g (n-1) 
          else generatecandidat_e t (n-1) 
      in generatecandidat_e (create_random_tree p)  n 

  (**
  @requires une liste de coordonnées base
  @ensures retourne une liste de couples de coordonnées correspondant aux arrêtes d'un arbre de Steiner pour les coordonnées de base
  @raises 
  *)
  let euclidian base = let t = gen_Steiner base (int_coord_list_to_float_coord_list base) 1000 
  in getbranches t