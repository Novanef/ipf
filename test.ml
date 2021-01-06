open Foncteur
open Projet
open Display

let read_coords =
    let rec aux acc n =
        if n = 0
        then List.rev acc
        else let c = Scanf.scanf "%d %d\n" (fun x y -> (x,y))
             in aux (c::acc) (n-1)
    in
    fun n -> aux [] n




module Tree_R = FoncteurTree(Coord_R)
open Tree_R
let f1 = Noeud(true,(1,2),[])
let f2 = Noeud(true,(4,5),[f1])
let f3 = Noeud(true,(2,3),[f2])
let t = create_tree [(0,0);(1,2);(2,3);(4,5)]
(* let k=generatecandidate t 3 *)

let rec print_tree t = match t with
Noeud(b,c,tl)->Printf.printf "Noeud(%b,(%d,%d),[" b (fst c) (snd c); print_tree_list tl;Printf.printf "])"
and print_tree_list tl = match tl with
[]->Printf.printf ""
|t::q-> Printf.printf ""; print_tree t;if q != [] then Printf.printf ";" else (); print_tree_list q; Printf.printf "\n"

(* let _ = print_tree k
let _ = print_tree t
let _ = Printf.printf "\n\n\n"; print_tree (add_n_edge t (getpoints t) 5)

let t= Noeud(true,(0,0),[Noeud(false,(1,0),[Noeud(false,(2,0),[Noeud(false,(2,2),[])
]);Noeud(false,(1,2),[])

]);Noeud(false,(0,2),[]);Noeud(true,(1,2),[Noeud(true,(2,3),[Noeud(false,(4,5),[])
])
]) *)

let _ = Random.self_init ()

open Foncteur open Projet open Projet.Tree_R
let t = create_tree [(0,0);(1,1);(2,2);]
(* let t = graphe_complet t;; *)

(* let t1 = (add_edge t t (1,1) (getpoints t) (getbase t)) *)
let cl = getcoordinates t 
let c = getrandom cl 
(* let t1 = (add_edge t t c (getpoints t) (getbase t)) *)
let t1 = add_random_edge t (getpoints t) (getbase t),true
let _ = print_tree t
let _= Printf.printf "\n________________________________________\n"
let _ = print_tree (fst t1) 
let _ = Printf.printf"\ncoord : "; dump_coord c; Printf.printf "\n"
let _ = Printf.printf "%b\n" (fst t1 = t);;

let l = Scanf.scanf "%d\n" (fun x -> x)
let _ = Printf.printf "l : %i\n%!" l
let l = Scanf.scanf "%d\n" (fun x -> x)
let _ = Printf.printf "l2 : %i\n%!" l

 open Foncteur open Projet open Projet.Tree_R;;
let t = create_tree [(1,1);(3,3);(0,0)];;
let g = graphe_complet t;;
