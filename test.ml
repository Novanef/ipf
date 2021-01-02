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
let t =
    let l = Scanf.scanf "%d\n" read_coords in
    create_tree l
(* let f1 = Noeud(true,(0,1),[])
let f2 = Noeud(true,(0,2),[])
let t = Noeud(true,(0,0),[f1;f2])
let f1 = Noeud(true,(1,2),[])
let f2 = Noeud(true,(4,5),[])
let f3 = Noeud(true,(2,3),[])
let t = Noeud(true,(0,0),[f1;f2]) *)
let k=generatecandidate t 5
let _=draw_rectilinear
(800,600)
(gettreepoints k)
(getbranches k)
