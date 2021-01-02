open Foncteur
module Tree_R = FoncteurTree(Coord_R)
open Tree_R
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
  (*50% de chance de supprimer un membre aléatoire de la liste*)
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
  
  (*Renvoie un arbre avec certaines arètes ont été aléatoirement supprimées*)
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

  (*itération n fois pour trouver un candidat avec un meilleur poids que l'arbre initial*)
  let generatecandidate t n = let p = getbase t in 
    let rec candidate t n = 
      if n > 0 then 
        let c = generatetree t in 
        if (findcycle c) && not(is_connexe c p) then 
          candidate t n 
        else 
          if (findcycle c) && (is_connexe c p) then 
          candidate c n 
          else 
            if not(findcycle c) &&( is_connexe c p) && (weight t) >= (weight (del_useless_branches c) ) then 
            candidate (del_useless_branches c) (n-1) 
            else 
            candidate t (n-1) 
      else t 
  in candidate (graphe_complet t) n

  (*Euclidien*)
  let gotopoint t p=let rec auxgoto t p l=match t with 
  |Noeud(b,c,tl)->if p=c then t::l else auxbgoto tl p l
  and auxbgoto tl p l=match tl with 
  |[]->l
  |r::q->auxgoto r p (l@(auxbgoto q p l)) in match (auxgoto t p []) with 
  |[]->failwith"no point p"
  |s::t->s
  (*let createtree p=let rec auxcreatetree p t l=match p with 
  |[]->t
  |r::q->let c=getrandom p in if l=[] then auxcreatetree (deletelist c p) (Noeud(true,c,[])) (c::l) 
  else match (gotopoint t (getrandom p)) with
  |Noeud(b,c,tl)->tl*)