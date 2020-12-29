module Tree=
struct
  type tree= 
  {
    coordinates : ((int*int)*(int*int)) list;
    weight : int;
  };;
  type ftree= 
    {
      coordinates : ((float*float)*(float*float)) list;
      weight : int;
    };;
  type candidate=
    {
    coordinates:(int*int)list;
    };; 
  type fcandidate=
    {
    coordinates:(float*float)list;
    };; 
  let abs x= if x>0 then x else -x;;
  (*retourne le poids selon la ditance de Manhattan d'un arbre donné*)
  let rec mweight t acc=match t with 
  |[]->acc
  |p::q->match p with (a,b),(c,d)->mweight q (acc+abs(a-c)+abs(b-d));;
  (*retourne le poids selon la norme 2 d'un arbre donné*)
  let rec uweight t acc=match t with 
  |[]->acc
  |p::q->match p with (a,b),(c,d)->uweight q (acc+.(a-.c)**2.+.(b-.d)**2.);;
  (*retourne la liste des abscisses et ordonnées des points*)
  let rec getcoordinates c x y=match c with 
  |[]-> x,y
  |p::q->match p with (a,b)->getcoordinates q (a::x) (b::y);;
  let rec auxgetpoints x y l=match y with
  |[]->l
  |p::q-> auxgetpoints x q ((x,p)::l);;
  (*retourne la liste des points possibles à partir des abscisses et ordonnées des points de départ dans le contexte des arbres rectilinéaires*)
  let rec getpoints l p=match l with 
  |([],y)->p
  |(d::q,y)->getpoints (q,y) ((auxgetpoints d y [])@p);;
    (*renvoie la liste des points possibles d'un arbre rectilinéaire*)
  let pointlist c= getpoints (getcoordinates c [] []) [];;
  let rec auxbranches c p k=match p with
  |[]->k
  |a::b->if a!=c then auxbranches c b ((c,a)::k) else auxbranches c b k;;
  let rec getbranches p k=match p with
  |[]->k
  |c::d-> getbranches d (auxbranches c p [])@k;;
  (*retourne la liste des branches possibles pour  la liste de points p donnée*)
  let branchlist p=getbranches p [];;
  end;;
