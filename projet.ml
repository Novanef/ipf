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
Printf.printf "\n erreur itération %i:%!" n; dump_coord coord1; dump_coord coord2; Printf.printf "\n%!" in ()
  
  
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
  (* let is_connexe t l=snd(is_accessible t l) *)
  


  (*Renvoie un arbre dont certaines arètes ont été aléatoirement supprimées*)
  let generatetree t=let u=getrandom (getpoints t)in let rec auxleaf t=match t with 
  |Noeud(b,c,tl)->if c=u then Noeud(b,c,(deleterandom tl)) else Noeud(b,c, (auxgen tl c))
  and auxgen tl c=match tl with
  |[]->[]
  |p::q->match p with 
  |Noeud(a,d,tlb)->if d!=c then (auxleaf (Noeud(a,d, tlb)))::(auxgen q d) else p::(auxgen q c) in auxleaf t


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

  let rec voisins_dispo t cl = match t with
    Noeud(b,c,tl)-> let v = voisins c cl cl  in 
    let tl_c = getcoord_treelist_nosubtree tl in
    let v_c = getcoord_treelist_nosubtree v in
    let rec remove_l1_from_l2 l1 l2 = match l2 with
    []->[]
    |t::q-> if isin t l1 then remove_l1_from_l2 l1 q else t::(remove_l1_from_l2 l1 q)
    in remove_l1_from_l2 tl_c v_c
  
  (** essaye d'ajouter une arête à t s'il n'est pas relié à un de ses voisins qui n'est pas r. renvoie t,true si t a une arête en plus, t,false sinon *)
  let rec try_add_edge t r cl base = match t with Noeud(_,c,_) ->  let v = voisins_dispo t cl in match r with
  Noeud(_,c_r,_) -> let v = deletelist c_r v in let v =deletelist c v in 
  if v =[] then t,false else
  let c_nt = getrandom v in match t with
  Noeud(b,c,tl) -> Noeud(b,c,Noeud(isin c_nt base,c_nt,[])::tl),true
 
  (* let try_add_edge t r cl base= let rec aux t r v = match r with
    Noeud(_,c_r,_) -> match t with
    Noeud(b,c,tl) -> match v with
      []->t,false
      |t1::q-> match t1 with
        Noeud(b_t1,c_t1,_) -> if not (c = c_t1) && not (c_r = c_t1) && not (isin c_t1 (getcoord_subtree_list t) ) then Noeud(b,c,Noeud(b_t1,c_t1,[])::tl),true else aux t r q
    in match t with
    Noeud(_,c,_) -> let v = change_bool_tree_list_nosubtree (voisins c cl cl) base in aux t r v *)

(** renvoie le sous-abre (au sens large) de t ayant comme sous-arbre direct un arbre de coordonnées c_st *)
  let rec get_racine t c_st = match t with
    Noeud(b,c,tl) -> if c = c_st || isin c_st (getcoord_subtree_list t) then t else get_racine_list tl c_st
    and get_racine_list tl c_st = match tl with
    []->failwith"pas trouvé"
    |t::q-> if isin c_st (getcoordinates t) then get_racine t c_st else get_racine_list q c_st

  (**ajoute au sous-arbre de t dont les coordonnées sont coord une arête si c'est possible *)
  let rec add_edge t big_r coord cl base = match t with
  Noeud(b,c,tl) -> if c = coord then try_add_edge t (get_racine big_r c) cl base else let res = add_edge_list tl big_r coord cl base in
  Noeud(b,c,fst res), snd res
  and add_edge_list tl big_r coord cl base = match tl with
  []->[],false
  |t::q -> let res = add_edge t big_r coord cl base in if snd res then (fst res)::q,true else let res = add_edge_list q big_r coord cl base
  in t::(fst res),snd res

  let rec add_random_edge t cl base =  let t_cl = getcoordinates t in let coord = getrandom t_cl in let try1 = add_edge t t coord cl base
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

  let rec add_n_edge t cl n base=  match n with
  0-> t
  |_-> add_n_edge (add_random_edge t cl base) cl (n-1) base

  (*itération n fois pour trouver un candidat avec un meilleur poids que l'arbre initial*)
  (* let generatecandidate t n = let p = getbase t in 
    let rec candidate t n bool= let cl = getpoints t in
      if n > 0 then let _ = Printf.printf "%i%!" n in
        let c = if bool then generatetree t else generatetree (add_n_edge t cl 2)  in 
        if (findcycle c) then
          if is_connexe c p then
            candidate c n false
          else
            candidate t n false
        else
          if is_connexe c p then
            if weight (del_useless_branches c) <= weight t then
              let _ = Printf.printf "on change d'abre%!" in
              candidate (del_useless_branches c) (n-1) false
            else
              let _ = Printf.printf "on garde le même arbre et pas :\n%!";print_tree (del_useless_branches c); Printf.printf "\n%!" in
              candidate t (n-1) false
          else
            candidate t n false
      else t 
  in candidate (graphe_complet t) n true *)
open Display

  let generatecandidate t n =   let p = getbase t in let cl = getpoints t in
    let rec candidate t c n m p cl=  
      if n <= 0   then t
      else 
        if (is_connexe c p) then 
          if (findcycle c) then
            candidate t (generatetree c) n (m+1) p cl
          else 
            let c2 = del_useless_branches c in
            if weight c2 <= weight t then
              candidate c2 (add_n_edge c2 cl 3 p) (n-1) (m+1) p cl
            else
              candidate t (add_n_edge t cl 3 p) (n-1) (m+1) p cl
        else 
          candidate t (add_n_edge c cl 1 p ) n (m+1) p cl

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
    in candidate (graphe_complet t) (graphe_complet t) n 1 p cl

  let rectilinear base = let t = generatecandidate (create_tree base) 1 in
  getbranches t

   (*Euclidien*)
   (**Relie le point p au noeud x de l'arbre t*)
    (*Euclidien*)

    module Tree_E = FoncteurTree(Coord_E)
    open Tree_E
    
    
    
        (**renvoie la liste des points relais, ie les points avec un bool false*)
        let getrelais t=let rec auxrelais t l=match t with
        |Noeud(b,c,tl)->if not(b) then auxrelaisb tl (c::l) else auxrelaisb tl l
        and auxrelaisb tl l=match tl with
        |[]->uniq l
        |p::q->auxrelaisb q (auxrelais p l) in auxrelais t [] 
    
    
       (**Relie le point p au noeud x de l'arbre t*)
       let rec putpoint x t p=match t with 
       |Noeud(b,c,tl)->if c=x then Noeud(b,c,(Noeud(true,p,[])::tl)) 
       else match tl with 
       |[]->Noeud(b,c,[])
       |r::q->Noeud(b,c,(putpoint x r p)::(auxputpoint x q p))
       and auxputpoint x q p=match q with
       |[]->[]
       |r::t->(putpoint x r p)::(auxputpoint x t p)
    
       let getrandom l=if(lengthlist l)>0 then let rec auxrand l x=match l with 
      |[]->failwith"liste vide2"
      |p::q->if x=0 then p else auxrand q (x-1) 
      in auxrand l (Random.int (lengthlist l)) else failwith"liste vide3"

      let rec float_coord_list l = match l with
      []->[]
      |t::q -> match t with (x,y) -> (float_of_int x,float_of_int y)::(float_coord_list q)
      
       (**relie les points de la liste p entre eux  (la liste l contient les points faisant parti de l'arbre pour les relier aux nouveaux points)*)
       let create_tree_e p=let rec auxcreatetree p t l=
       if not(p=[]) then let c=getrandom p in if l=[] then auxcreatetree (deletelist c p) (Noeud(true,c,[])) (c::l) 
       else let k=getrandom l in auxcreatetree (deletelist c p) (putpoint k t c) (c::l) else t
       in auxcreatetree p (Noeud(false,(0.0,0.0),[])) []
    
       (**retourne un couple de booléen correspondant à la comparaison des coordonnées de 2 points, true si p1 strictement sup à p2*)
      let compare p1 p2=match p1,p2 with
      |(a,b),(x,y)->if a>x&&b>y then true,true else if a>x&&b<=y then true,false
      else if a<=x&&b>y then false,true else false,false
      
       (**renvoie un point avec les coordonnées minimales de la liste*)
       let getmin l=match l with
       |[]->failwith"listevide4"
       |p::q-> let rec auxmin min l=match l with 
       |[]->min
       |p::q->let c=compare min p in if (fst c)&&(snd c) then auxmin p q 
       else if not(fst c)&&(snd c) then auxmin (fst min,snd p) q
       else if (fst c)&&not(snd c) then auxmin (fst p,snd min) q
       else auxmin min q in auxmin p l
    
       (**renvoie un point avec les coordonnées maximales de la liste*)
       let getmax l=match l with
       |[]->failwith"listevide5"
       |p::q-> let rec auxmax max l=match l with 
       |[]->max
       |p::q->let c=compare max p in if (fst c)&&(snd c) then auxmax max q 
       else if not(fst c)&&(snd c) then auxmax (fst p,snd max) q
       else if (fst c)&&not(snd c) then auxmax (fst max,snd p) q
       else auxmax p q in auxmax p l
    
      let sign p1 p2 p3=match p1,p2,p3 with
      |(a,b),(c,d),(x,y)->((a-.x)*.(d-.y))-.((c-.x)*.(b-.y))
    
      (**retourne true si le point p est dans le triangle v1v2v3*)
      let isintriangle p v1 v2 v3=let res1,res2,res3=(sign p v1 v2),(sign p v2 v3),(sign p v3 v1) in
      not((res1<0.||res2<0.||res3<0.)&&(res1>0.||res2>0.||res3>0.))
      
      (**retourne une liste de 3 points parmi ceux de l'arbre*)
      let gettriangle t=let rec auxtriangle n r l=if n>0 then let p=getrandom l in auxtriangle (n-1) (p::r) (deletelist p l) 
      else r
      in auxtriangle 3 [] (gettreepoints t)
    
      let pop l=match l with 
      |[]->failwith"liste vide7"
      |p::q->p,q
    
      (**retourne la liste d'arètes du noeud t sans les noeuds compris dans l*)
      let checkbranches t l=match t with 
      |Noeud(b,c,tl)->let rec auxcheck tl l=match tl with 
      |[]->[]
      |p::q -> match p with 
      |Noeud(a,d,tll)->if not(mem l d) then auxcheck q l else (Noeud(a,d,tll))::(auxcheck q l) in auxcheck tl l 
      
      (**retourne le sous arbre de coordonnées p*)
      let subtree p t=let rec getsubtree p t l=match t with 
      |Noeud(b,c,tl)->if c=p then Noeud(b,c,tl)::l else auxgoto p tl l
      and auxgoto p tl l=match tl with
      |[]->l
      |r::q->(getsubtree p r l)@(auxgoto p q l) in match (getsubtree p t []) with
      |[]->failwith"not found"
      |p::q-> p
    
      (**renvoie les coordonées des points auxquels la racine de t est reliée*)
      let getbranchcoord t=match t with
      |Noeud(b,c,tl)->let rec auxbranchcoord tl l=match tl with
      |[]->l
      |p::q->match p with 
      |Noeud(b,c,tl)->auxbranchcoord q (c::l) in auxbranchcoord tl []
      (**renvoie les coordonnées d'un point dans le triangle formé par les coordonnées de 3 points de t aléatoires*)
      let genpoint tri =let max,min=getmax tri,getmin tri in let rec auxgenpoint p1 p2 p3= let randpoint max min=
      let p=(Random.float (fst (max)-.fst (min))+.fst min,Random.float(snd(max)-.snd(min))+.snd(min)) in if (isintriangle p p1 p2 p3) then p 
      else auxgenpoint p1 p2 p3 in randpoint max min in auxgenpoint (fst(pop tri)) (fst(pop (snd(pop tri)))) (fst(pop(snd(pop(snd(pop tri))))))
     
      (**retourne la liste des sous arbres de t de coordonnées contenues dans l*)
      let subtreelist t l=let rec auxsub t l k=match l with
      |[]->k
      |p::q-> auxsub t q ((subtree p t)::k) in auxsub t l [] 
       
      (**retourne une liste de sous arbres qui ne sont pas directement reliés entre eux*)
      let rec checktriangle tl l=match tl with 
      |[]->[]
      |p::q->match p with
      Noeud(b,c,tll)-> (Noeud(b,c, (checkbranches p l) ))::(checktriangle q l)
    
      let addtree t p l=Noeud(false,p,checktriangle (subtreelist t l) l)  
    
      (**ajoute un point généré aléatoirement dans le triangle formé par 3 points choisi aléatoirement parmi ceux de t
      puis supprime les arètes entre ces 3 points et les relie à p*)
      let addtotree_e t=if(lengthlist (gettreepoints t))<3 then failwith"moins de 3 points" 
      else let l=(gettriangle t) in if (lengthlist l)<3 then failwith"??" else let pt=genpoint l in let rec auxadd t p l=match p with
      |Noeud(b,c,tl)->if not(mem l c) then addtree t pt l,true else Noeud(b,c,auxbadd tl l),false
      and auxbadd tl l=match tl with 
      |[]->[]
      |p::q->if snd(auxadd t p l) then fst(auxadd t p l)::q else p::(auxbadd q l) in fst(auxadd t t l)
    
    
    
      (**change la position d'un point relais aléatoire*)
      let movepoint t=let rec auxmove s p=match s with
      |Noeud(b,c,tl)->if c=p then Noeud(b,genpoint (gettriangle t),tl) else Noeud(b,c,auxbmove tl p)
      and auxbmove tl p=match tl with
      |[]->[]
      |r::q->(auxmove r p)::(auxbmove q p)  in auxmove t (getrandom (getrelais t))
    
      (**fusionne les listes l et s sans doublons*)
      let rec mergelists l s=match l with 
      |[]-> s
      |p::q->if mem s p then mergelists q (p::s) else mergelists q s
    
      (**retourne le noeud p de contenu dans les arêtes de t et fusionne les arêtes de p avec la liste l*)
      let rec merge p l=match p with 
      |Noeud(b,c,tl)->Noeud(b,c,mergelists l tl)
    
      (**retourne les coordonnées du noeud courant*)
      let getcurrcoord t=match t with 
      |Noeud(b,c,tl)->c
      (**tire un point relais au hasard et donne ses arêtes à un point aléatoire auquel il est relié*)
      let mergepoint t=let rec auxmerge prec s p= match s with 
      |Noeud(b,c,tl)->if c=p then if tl=[] then (subtree prec t) else let pt=subtree (getrandom (getbranchcoord s)) s in (merge pt (deletelist pt tl)) else Noeud(b,c,auxbmerge c tl p)
      and auxbmerge prec tl p=match tl with
      |[]->[]
      |r::q->(auxmerge prec r p)::(auxbmerge prec q p)  in auxmerge (getcurrcoord t) t (getrandom (getrelais t))
    
      (**renvoie une liste contenant un seul arbre différente de coordonnées du point p*)
      let replace p tl t=match tl with 
      |[]->[]
      |r::q->match r with 
      |Noeud(b,c,tll)->let pt=getrandom((deletelist c (deletelist p (gettreepoints t)))) in [subtree pt t]
    
          (**renvoie le "sur-abre" de p dans t *)
    let get_pretree t p =
      let rec aux t p =  match t with
      Noeud(_,c,tl) -> let c_tl = getcoord_treelist_nosubtree tl in 
        if isin p c_tl then 
          c,true 
        else
          aux_list tl p
      and aux_list tl p = match tl with
      []->(zero,zero),false
      |t::q -> let try1 = aux t p in if snd try1 then try1 else aux_list q p
      in let try1 = aux t p in if snd try1 then fst try1 else failwith"le point n'est pas dans l'abre"
      
      
      let get_p_r t p_b = match t with
      Noeud(b,c,tl) -> if c = p_b then match tl with 
      st::[] -> begin match st with Noeud(_,c_st,_) -> getrandom (deletelist c_st (getcoordinates t) ) end
      |_->failwith"mauvais arbre ou mauvais point de base"
      else
      getrandom (deletelist (get_pretree t p_b) (getcoordinates t) )
      
      let add_p_b_to_p_r t p_b p_r = 
      let rec aux t p_b p_r = match t with
      Noeud(b,c,tl) -> if c = p_r then Noeud(b,c,Noeud(true,p_b,[])::tl),true
      else let res = aux_list tl p_b p_r in Noeud(b,c,fst res),snd res
      and aux_list tl p_b p_r = match tl with
      []->[],false
      |t::q-> let try1 = aux t p_b p_r in if snd try1 then (fst try1::q),true else aux_list q p_b p_r
      in let try1 = aux t p_b p_r in if snd try1 then fst try1 else failwith"p_r introuvable dans t"
      
      let del_p_b t p_b = 
      let rec aux_list tl p_b = match tl with
      []->[]
      |t::q-> let n_t = aux t p_b in 
        match n_t with 
        Noeud(b,c,tl_tl) -> 
          if c = p_b then 
            aux_list q p_b
          else t::(aux_list q p_b)
      and aux t p_b = match t with 
        Noeud(b,c,tl) -> Noeud(b,c,aux_list tl p_b)
      in aux t p_b
      
      let change_edge t p_b = let p_r = get_p_r t p_b in match t with
      Noeud(_,c,tl) -> if c = p_b then match tl with
      st::[] -> add_p_b_to_p_r st p_b p_r
      |_ ->failwith"mauvais arbre"
      else
      add_p_b_to_p_r (del_p_b t p_b) p_b p_r
          (**retourne true si un sous arbre de t autre que celui de coordonnées r est relié à p*)
          let checksubtree t p r=let rec checkaux t p l=match l with
          |[]->false
          |r::q->(checkbaux t r p q)||(checkaux t p q)
          and checkbaux t s p q=match (subtree s t) with
          Noeud(b,c,tl)->if isin p tl  then true else checkaux t p q in checkaux t (subtree p t) (deletelist r (gettreepoints t))

      (*retourne la base de l'arbre, ie les points avec un bool=true, qui n'ont qu'une seule arête*)
      let getbase1 t=let rec auxbase s l prec=match s with
      |Noeud(b,c,tl)->if((b)&&(lengthlist tl=1))||((b)&&tl=[]&&not(checksubtree t c prec)) then auxbaseb tl (c::l) c else auxbaseb tl l c
      and auxbaseb tl l prec=match tl with
      |[]->uniq l
      |p::q->auxbaseb q (auxbase p l prec) prec in auxbase t [] (getcurrcoord t)
      (**recherche un point de départ (bool true) avec une seule arête et la change*)
      let newbranch_e t=let rec auxnew s p=match s with
      |Noeud(b,c,tl)->if c=p then if lengthlist tl=1 then Noeud(b,c,replace p tl t) else Noeud(b,c,tl) else Noeud(b,c,auxbnew tl p)
      and auxbnew tl p=match tl with
      |[]->[]
      |r::q->(auxnew r p)::(auxbnew q p)  in auxnew t (getrandom (getbase1 t))

      let newbranch t=change_edge t (getrandom (getbase1 t))
    
      let randomchange t=let r=if (lengthlist (getrelais t)=0) then 0 else Random.int 4 in match r with
      |0->addtotree_e t
      |1->if (lengthlist (getrelais t)>0) then mergepoint t else t
      |2->if (lengthlist (getrelais t)>0) then movepoint t else t
      |3-> if (lengthlist (getbase1 t)>0) then newbranch t else t
      |_->failwith"impossible"
      
      let generatecandidate_e t n= let p = getbase t 
      in 
      if (lengthlist p)<3 then 
      t 
      else let rec generatecandidat_e t n=if n=0 then t else let g=randomchange t in 
        if n>0 then
          if (is_connexe g p) then 
            if (findcycle g) then
              if(weight t)>=(weight g) then 
                generatecandidat_e g (n-1)
              else generatecandidat_e t (n-1)
            else generatecandidat_e t (n-1)
          else generatecandidat_e t (n-1)
        else t 
      in generatecandidat_e t n