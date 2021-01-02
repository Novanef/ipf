open Foncteur
module Tree_R = FoncteurTree(Coord_R)
open Tree_R
  (*supprime le membre égal à p de la liste*)
  let rec deletelist p l=match l with 
  |[]->[]
  |r::q->if r=p then q else r::(deletelist p q)
  (*retourne la base de l'arbre, ie les points avec un bool=true*)
  let getbase t=let rec auxbase t l=match t with
  |Noeud(b,c,tl)->if(b=true) then auxbaseb tl (c::l) else auxbaseb tl l
  and auxbaseb tl l=match tl with
  |[]->uniq l
  |p::q->auxbaseb q (auxbase p l) in auxbase t [] 
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

  (*itération n fois pour trouver un candidat avec un meilleur poids que l'arbre initial*)
  let generatecandidate t n=let p=getbase t in let rec candidate t n=if n>0 then let c=(generatetree t)in if findcycle c||not(is_connexe c p) then candidate t n 
  else if (weight t)>=(weight c) then candidate c (n-1) else candidate t (n-1) else t in candidate t n