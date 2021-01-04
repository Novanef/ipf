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




module Tree_R = FoncteurTree(Coord_R)
open Tree_R
let _ = Printf.printf "ALED";;
let t =
    let l = Scanf.scanf "%d\n" read_coords in
    create_tree l
let k=generatecandidate t 1
let _ = Printf.printf "\ncyclique : %b\n" (findcycle k)
let _ = Printf.printf "connexe : %b\n" (is_connexe k (getbase t))
let _=draw_rectilinear
(800,600)
(gettreepoints k)
(getbranches k)