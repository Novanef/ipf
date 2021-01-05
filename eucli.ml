open Foncteur
open Projet
open Display
let _ = Random.self_init ()
let read_coords =
    let rec aux acc n =
        if n = 0
        then List.rev acc
        else let c = Scanf.scanf "%d %d\n" (fun x y -> (x,y))
             in aux (c::acc) (n-1)
    in
    fun n -> aux [] n

module Tree_E = FoncteurTree(Coord_E)
open Tree_E
let t =
    let l =  Scanf.scanf "%d\n" read_coords in
    create_tree_e l
let k=generatecandidat_e t 100
let _=draw_eulcidean
(800,600) 
(getbase k)
(getbranches k)