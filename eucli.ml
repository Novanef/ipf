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

let l =  sort_list (Scanf.scanf "%d\n" read_coords)
let k=generatecandidate_e (float_coord_list l) 10
let _=Printf.printf "%f\n" (weight k)
let _=draw_eulcidean
(800,600) 
(l)
(getbranches k)