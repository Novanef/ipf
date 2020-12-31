open Foncteur
module Tree_R = FoncteurTree(Coord_R)
open Tree_R
  let rec deletelist p l=match l with 
  |[]->[]
  |r::q->if r=p then q else p::(deletelist p q)
  let lengthlist l=let rec aux l acc=match l with 
  |[]->acc
  |p::q-> aux q (acc+1) in aux l 0
  let deleterandom l=if(lengthlist l)>0 then let rec auxrand l x=match l with 
  |[]->[]
  |p::q->if x=0&&(Random.int 2=1) then q else p::(auxrand q (x-1)) 
  in auxrand l (Random.int (lengthlist l)) else []
  let is_connexe t l=match t with 
  |Noeud(b,c,tl)->let rec is_accessible c t l=if not(mem l c) then auxaccessible c tl (deletelist c l) else auxaccessible c tl l
  and auxaccessible p tl l=match tl with
  |[]-> l=[]
  |s::u->is_accessible p s l||l=[] 
  in is_accessible c t []

  
  let rec  generatetree t=match t with 
  |Noeud(b,c,tl)->Noeud(b,c, (auxgen (deleterandom tl) c))
  and auxgen tl c=match tl with
  |[]->[]
  |p::q->match p with 
  |Noeud(a,d,tlb)->if d!=c then (generatetree (Noeud(a,d, tlb)))::(auxgen q d) else p::(auxgen q c)