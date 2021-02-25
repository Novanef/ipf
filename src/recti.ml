open Tree
module Rectilinear_Tree = TreeMod(Rectilinear_Coord)
open Rectilinear_Tree

  (** fonction de débogage
  @requires une coordonée c et une liste d'arbres tl
  @ensures retourne true si la coordonnée de la racine d'un arbre de tl n'a ni  abscisse ni ordonnée en commun avec c
  @raises 
  *)
  let rec is_coord_out_of_place (x,y) tl = match tl with
    []->false,(zero,zero),(zero,zero)
    |Node(_,(x2,y2),_)::q-> if (x != x2) && (y != y2) then 
      true,(x,y),(x2,y2) 
    else 
      is_coord_out_of_place (x,y) q

  (** fonction de débogage
  @requires un arbre t
  @ensures si t contient un arbre dont la racine n'a ni abscisse ni ordonnée en commun avec la coordonée de la racine d'un de ses sous-arbres, renvoie true ainsi que les deux coordonnées en question,sinon renvoie false,(0,0),(0,0)
  @raises 
  *)
  let rec check_error t = match t with
    Node(b,c,tl)-> let test = is_coord_out_of_place c tl 
    in match test with (b,_,_) -> 
      if b then 
        test 
      else
        check_error_list tl
  and check_error_list tl = match tl with
    []->false,(zero,zero),(zero,zero)
    |t::q-> let test = check_error t 
    in match test with (b,_,_) -> 
      if b then
        test 
      else
        check_error_list q

  (** fonction de débogage
  @requires un arbre c et un entier n
  @ensures si le premier élément de check_error c est true, affiche dans la sortie standard l'entier ainsi que les deux coordonées renvoyées par check_error c
  @raises 
  *)
  let test c n = let testos = check_error c in 
  match testos with 
  (b,c1,c2) -> if b then 
    let _ = Printf.printf "\n erreur itération %i" n; dump_coord c1; dump_coord c2; Printf.printf "\n"; dump_tree c;Printf.printf "\n%!" in ()

  (**
  @requires une liste l
  @ensures retourne l avec un de ses éléments pris au hasard en moins, ou l si l est vide
  @raises 
  *)
  let deleterandom l = if(List.length l) > 0 then 
    let rec auxrand l x = match l with 
      |[]->[]
      |p::q-> if x = 0 then 
        q 
      else 
        p::(auxrand q (x-1)) 
    in auxrand l (Random.int (List.length l)) 
  else 
    []

  (**
  @requires un arbre t
  @ensures retourne t dont un sous-arbre (au sens large, t non inclus) a été supprimé
  @raises 
  *)
  let generatetree t = 
  let u = getrandom (getpoints t) in 
  let rec auxleaf t = match t with 
    Node(b,c,tl)->if c = u then
      Node(b,c,(deleterandom tl)) 
    else
      Node(b,c, (auxgen tl c))
  and auxgen tl c = match tl with
    |[]->[]
    |p::q->match p with 
    |Node(a,d,tlb)->
      if d != c then 
        (auxleaf (Node(a,d, tlb)))::(auxgen q d) 
      else 
        p::(auxgen q c) 
  in auxleaf t

  (**
  @requires une liste de coordonées p et un arbre t tels que is_connected t p retourne true
  @ensures retourne t dont l'un de ses sous-arbres (au sens large, t non inclus) a été supprimé tel que is_connected t p soit toujours vrai
  @raises failure "del_branches : arbre non connexe" si is_connected t p retourne false
  *)
  let rec del_branches t p = if not (is_connected t p) then 
    failwith "del_branches : arbre non connexe" 
    else
      let test = generatetree t 
      in if (is_connected test p) then 
        test 
      else 
        del_useless_branches (del_branches t p)

  (**
  @requires un arbre t et une liste de coordonnées cl
  @ensures la liste des coordonnées voisines de celle de la racine de t c au sens de neighbors c cl cl, moins les coordonnées déjà présentes dans la liste de celles des racines des sous-arbres (direct, t non inclus) de t
  @raises 
  *)
  let rec available_neighbors t cl = match t with
  Node(b,c,tl)-> let v = neighbors c cl cl  in 
  let tl_c = getcoord_treelist_nosubtree tl in
  let v_c = getcoord_treelist_nosubtree v in
  let rec remove_l1_from_l2 l1 l2 = match l2 with
    []->[]
    |t::q-> if isin t l1 then 
      remove_l1_from_l2 l1 q 
    else 
      t::(remove_l1_from_l2 l1 q) 
  in remove_l1_from_l2 tl_c v_c
  
  (**
  @requires deux arbres t et r, deux listes de coordonnées cl et base
  @ensures si available_neighbors t cl contient une autre coordonnées à part celles des racines de t et r, en prend une c_nt au hasard qui n'est ni celle de r ni celle de t et retourne t dont un Node(isin c_nt base,c_nt,[]) a été ajouté à la liste de ses sous-arbres,true; retourne t,false sinon
  @raises 
  *)  
  let rec try_add_edge t r cl base = match t with 
    Node(_,c,_) ->  let v = available_neighbors t cl 
    in match r with
      Node(_,c_r,_) -> let v = deletelist c_r v 
      in let v = deletelist c v in 
      if v = [] then 
        t,false 
      else
      let c_nt = getrandom v  
      in match t with
        Node(b,c,tl) -> Node(b,c,Node(isin c_nt base,c_nt,[])::tl),true


  (**
  @requires deux arbres t et big_r, une coordonée coord et deux listes de coordonées cl et base
  @ensures retourne t dont le sous-arbre (au sens large, t inclus) de racine de coordonnée coord a été remplacé par try_add_edge t (get_root big_r c) cl base ainsi que le booléen de try_add_edge t (get_root big_r c) si t contient un tel sous-arbre. Renvoie t,false sinon
  @raises 
  *)
  let rec add_edge t big_r coord cl base = match t with
    Node(b,c,tl) -> if c = coord then 
      try_add_edge t (get_root big_r c) cl base 
    else 
      let res = add_edge_list tl big_r coord cl base in
      Node(b,c,fst res), snd res
  and add_edge_list tl big_r coord cl base = match tl with
    []->[],false
    |t::q -> let res = add_edge t big_r coord cl base 
    in if snd res then 
      (fst res)::q,true 
    else 
      let res = add_edge_list q big_r coord cl base
      in t::(fst res),snd res

  (**
  @requires un arbre t et deux listes de coordonnées cl et base
  @ensures retourne t dont un sous-arbre (au sens large, t inclus) pris au hasard a lui-même un sous-arbre en plus comme ajouté dans add_edge t t (coord_prises_au_hasard) cl base
  @raises 
  *)
  let rec add_random_edge t cl base =  let t_cl = getcoordinates t 
  in let coord = getrandom t_cl 
  in let try1 = add_edge t t coord cl base
  in if snd try1 then 
    fst try1 
  else 
    add_random_edge t cl base

  (**
  @requires un arbre t, un entier positif n et deux listes de coordonnées cl et base
  @ensures retourne t à qui on a appliqué add_random_edge t cl base n fois 
  @raises 
  *)
  let rec add_n_edge t cl n base =  match n with
  0-> t
  |_-> add_n_edge (add_random_edge t cl base) cl (n-1) base

    (**
  @requires un arbre t et un entier n
  @ensures retourne le plus petit (au sens de weight t) de n arbres acycliques reliés à toutes les coordonnées de la base générés aléatoirement
  @raises 
  *)
  let gen_Steiner t n =   let p = getbase t in let cl = getpoints t in
    let rec candidate t c n p cl =
      if n <= 0 then t
      else 
          if (findcycle c) then
            candidate t (del_branches c p) n p cl
          else 
            let c2 = del_useless_branches c in
            if weight c2 <= weight t then
              candidate c2 (add_n_edge c2 cl (int_of_float (sqrt (float_of_int (List.length cl)))) p) (n-1) p cl
            else
              candidate t (add_n_edge t cl 6 p) (n-1) p cl

     in candidate (complete_graph t) (complete_graph t) n p cl

  (**
  @requires une liste de coordonnées base
  @ensures retourne une liste de couples de coordonnées correspondant aux arrêtes d'un arbre de Steiner pour les coordonnées de base
  @raises 
  *)
  let rectilinear base = let t = gen_Steiner (create_tree base) 1000 
  in getbranches t

