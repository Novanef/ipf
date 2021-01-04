open Foncteur
module Tree_R = FoncteurTree(Coord_R)
open Tree_R

    let rec is_coord_out_of_place c tl = match tl with
  []->false,(zero,zero),(zero,zero)
  |t::q-> match t with Noeud(_,(x2,y2),_) -> match c with (x,y) -> if (x!=x2) && y!= y2 then true,(x,y),(x2,y2) else is_coord_out_of_place c q

  let rec check_error t = match t with
  Noeud(b,c,tl)-> let test = is_coord_out_of_place c tl in match test with (b,_,_) -> if b then test else
  check_error_list tl
  and check_error_list tl = match tl with
  []->false,(zero,zero),(zero,zero)
  |t::q-> let test = check_error t in match test with (b,_,_) -> if b then test else check_error_list q

let test c n= 
let testos = check_error c in 
match testos with 
(b,coord1,coord2) ->if b then let _ =
Printf.printf "\n erreur itération %i:" n; dump_coord coord1; dump_coord coord2; Printf.printf "\n" in ()

  (*supprime le membre égal à p de la liste*)
  let rec deletelist p l=match l with 
  |[]->[]
  |r::q->if r=p then q else r::(deletelist p q)

  let gettreepoints t=let rec auxtreepoints t l=match t with
  |Noeud(b,c,tl)->auxtreepointsbis tl (uniq (c::l)) 
  and auxtreepointsbis tl l=match tl with 
  |[]->l
  |p::q->uniq ((auxtreepoints p l)@(auxtreepointsbis q l)) in auxtreepoints t []
  
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
  (*retourne la longueur de la liste*)
  let lengthlist l=let rec aux l acc=match l with 
  |[]->acc
  |p::q-> aux q (acc+1) in aux l 0

  (*supprime un membre aléatoire de la liste*)
  let deleterandom l=if(lengthlist l)>0 then let rec auxrand l x=match l with 
  |[]->[]
  |p::q->if x=0 then q else p::(auxrand q (x-1)) 
  in auxrand l (Random.int (lengthlist l)) else []

  let getrandom l=if(lengthlist l)>0 then let rec auxrand l x=match l with 
  |[]->failwith"liste vide"
  |p::q->if x=0 then p else auxrand q (x-1) 
  in auxrand l (Random.int (lengthlist l)) else failwith"liste vide"

  (*retourne la liste de coordonnées l moins les points trouvés lors du parcours de t*)
  let rec is_accessible t l=match t with 
  |Noeud(b,c,tl)->if not(mem l c) then auxaccessible tl (deletelist c l) else auxaccessible tl l
  and auxaccessible tl l=match tl with
  |[]-> l,l=[]
  |s::u->fst(is_accessible s l),snd(auxaccessible u (fst(is_accessible s l)))||l=[]

  (*retourne true si tous les points de la liste l ont été trouvés*)
  let is_connexe t l=snd(is_accessible t l)
  
  (*Renvoie un arbre dont certaines arètes ont été aléatoirement supprimées*)
  let generatetree t=let u=getrandom (getpoints t)in let rec auxleaf t=match t with 
  |Noeud(b,c,tl)->if c=u then Noeud(b,c,(deleterandom tl)) else Noeud(b,c, (auxgen tl c))
  and auxgen tl c=match tl with
  |[]->[]
  |p::q->match p with 
  |Noeud(a,d,tlb)->if d!=c then (auxleaf (Noeud(a,d, tlb)))::(auxgen q d) else p::(auxgen q c) in auxleaf t

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

  (**renvoie les coordonnées des sous-arbres directs de t*)
  let getcoord_subtree_list t = let rec aux tl = match tl with
  []->[]
  |t::q-> match t with
    Noeud(_,c,_) -> c::(aux q)
  in match t with
  Noeud(_,_,tl) -> aux tl

  (** change les booléens de tous les abres de la liste tl (pas leur sous-arbres) selon si leur coordonnées sont dans cl ou non *)
  let rec change_bool_tree_list_nosubtree tl cl = match tl with
    []->[]
    |t::q-> match t with
    Noeud(b,c,tl)-> if isin c cl then Noeud(true,c,tl)::(change_bool_tree_list_nosubtree q cl) else  Noeud(false,c,tl)::(change_bool_tree_list_nosubtree q cl)

  (** essaye d'ajouter une arête à t s'il n'est pas relié à un de ses voisins qui n'est pas r. renvoie t,true si t a une arrête en plus, t,false sinon *)
  let try_add_edge t r cl base= let rec aux t r v = match r with
    Noeud(_,c_r,_) -> match t with
    Noeud(b,c,tl) -> match v with
      []->t,false
      |t1::q-> match t1 with
        Noeud(b_t1,c_t1,_) -> if not (c = c_t1) && not (c_r = c_t1) && not (isin c_t1 (getcoord_subtree_list t) ) then Noeud(b,c,Noeud(b_t1,c_t1,[])::tl),true else aux t r q
    in match t with
    Noeud(_,c,_) -> let v = change_bool_tree_list_nosubtree (voisins c cl cl) base in aux t r v

(** renvoie le sous-abre (au sens large) de t ayant comme sous-arbre direct un arbre de coordonnées c_st *)
  let rec get_racine t c_st = match t with
    Noeud(b,c,tl) -> if c = c_st || isin c_st (getcoord_subtree_list t) then t else get_racine_list tl c_st
    and get_racine_list tl c_st = match tl with
    []->failwith"pas trouvé"
    |t::q-> if isin c_st (getcoordinates t) then get_racine t c_st else get_racine_list q c_st

  (**ajoute au sous-arbre de t dont les coordonnées sont coord une arrête si c'est possible *)
  let rec add_edge t big_r coord cl base = match t with
  Noeud(b,c,tl) -> if c = coord then try_add_edge t (get_racine big_r c) cl base else let res = add_edge_list tl big_r coord cl base in
  Noeud(b,c,fst res), snd res
  and add_edge_list tl big_r coord cl base = match tl with
  []->[],false
  |t::q -> let res = add_edge t big_r coord cl base in if snd res then (fst res)::q,true else let res = add_edge_list q big_r coord cl base
  in t::(fst res),snd res

  let rec add_random_edge t cl base= let cl = getcoordinates t in let coord = getrandom cl in let try1 = add_edge t t coord cl base
  in if snd try1 then fst try1 else add_random_edge t cl base

  (* *ajoute une arête dans l'arbre t dans le contexte de cl si c'est possible sans dédoubler d'arête
  let add_edge t cl base= 
    let rec aux t r cl = let try1 = try_add_edge t r cl base in 
      if snd try1 then 
        fst try1 
      else 
        match t with
          Noeud(b,c,tl) -> Noeud(b,c,aux_list tl t cl)
    and aux_list tl r cl = match tl with
      []->[]
      |t::q-> let try1 = aux t r cl in 
        if not (try1 = t) then 
          try1::q 
        else 
          t::(aux_list q r cl)
    in aux t t cl

  let rec add_random_edge t cl base =
    match t with
    Noeud(b,c,tl) -> let rand = Random.int 3 in if rand = 1 || tl = [] then add_edge t cl base else Noeud(b,c,add_random_edge_list tl cl base)
    and add_random_edge_list tl cl base = match tl with
     []->[]
     |t::q-> let rand = Random.int 3 in if rand = 1 then (add_random_edge t cl base)::q else t::(add_random_edge_list q cl base)  *)

  let rec add_n_edge t cl n base= match n with
  0-> t
  |_-> add_n_edge (add_random_edge t cl base) cl (n-1) base

  (*itération n fois pour trouver un candidat avec un meilleur poids que l'arbre initial*)
  (* let generatecandidate t n = let p = getbase t in 
    let rec candidate t n bool= let cl = getpoints t in
      if n > 0 then let _ = Printf.printf "%i" n in
        let c = if bool then generatetree t else generatetree (add_n_edge t cl 2)  in 
        if (findcycle c) then
          if is_connexe c p then
            candidate c n false
          else
            candidate t n false
        else
          if is_connexe c p then
            if weight (del_useless_branches c) <= weight t then
              let _ = Printf.printf "on change d'abre" in
              candidate (del_useless_branches c) (n-1) false
            else
              let _ = Printf.printf "on garde le même arbre et pas :\n";print_tree (del_useless_branches c); Printf.printf "\n" in
              candidate t (n-1) false
          else
            candidate t n false
      else t 
  in candidate (graphe_complet t) n true *)



  let generatecandidate t n=  let _ = Printf.printf "début" in let p = getbase t in let cl = getpoints t in
    let rec candidate t c n = let _ = test c n in let _ = Printf.printf "%i" n in
      if n = 0 then t
      else 
        if (is_connexe c p) then
          if (findcycle c) then let _ = Printf.printf "cycle trouvé \n" in
            candidate t (generatetree c) n
          else let _ = Printf.printf ("\non avance\n\n") in 
            let c2 = del_useless_branches c in
            if weight c2 <= weight t then
              candidate c2 (add_n_edge c2 cl 3 p) (n-1)
            else
              candidate t (add_n_edge t cl 3 p) (n-1)
        else let _ = Printf.printf "pas connexe\n" in
          candidate t (add_n_edge c cl 1 p ) n

        (* if (findcycle c) then
          candidate t (generatetree c) n
        else
          if (is_connexe c p) then let c2 = del_useless_branches c in
            if weight c2 <= weight t then 
              candidate c2 (add_n_edge c2 cl 3 p) (n-1)
            else
              candidate t (add_n_edge t cl 3 p) (n-1)
          else
            candidate t (add_n_edge c cl 1 p) n *)
    in candidate (graphe_complet t) (graphe_complet t) n



   (*Euclidien*)
   (**Relie le point p au noeud x de l'arbre t*)
   let rec putpoint x t p=match t with 
   |Noeud(b,c,tl)->if c=x then Noeud(b,c,(Noeud(true,p,[])::tl)) 
   else match tl with 
   |[]->Noeud(b,c,[])
   |r::q->Noeud(b,c,(putpoint x r p)::(auxputpoint x q p))
   and auxputpoint x q p=match q with
   |[]->[]
   |r::t->(putpoint x r p)::(auxputpoint x t p)

   (**relie les points de la liste p entre eux  (la liste l contient les points faisant parti de l'arbre pour les relier aux nouveaux points)*)
   let createtree p=let rec auxcreatetree p t l=match p with 
   |[]->t
   |r::q->let c=getrandom p in if l=[] then auxcreatetree (deletelist c p) (Noeud(true,c,[])) (c::l) 
   else let k=getrandom (c::l) in auxcreatetree (deletelist c p) (putpoint k t c) (c::l)
   in auxcreatetree p (Noeud(false,(0,0),[])) []