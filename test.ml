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
Noeud(b,c,tl)->Printf.printf "Noeud(%b,(%i,%i),[" b (fst c) (snd c); print_tree_list tl;Printf.printf "])"
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
let t = create_tree [(0,0);(1,1);(2,2);(3,3)]
let g = graphe_complet t;;
let _ = test g 0;;
let t1 = get_racine t (0,0)
let _ = print_tree t1