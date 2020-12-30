open Foncteur

let dump_coord (x,y) =
    Printf.printf "(%d,%d) " x y

let dump l =
    let _ = Printf.printf "{ " in
    let _ = List.iter dump_coord l in
    Printf.printf "}\n%!"

module Tree_R = FoncteurTree(Coord_R)
open Tree_R
let f1 = Noeud(true,(0,1),[])
let f2 = Noeud(true,(0,2),[])
let t = Noeud(true,(0,0),[f1;f2])
let f1 = Noeud(true,(1,2),[])
let f2 = Noeud(true,(4,5),[])
let f3 = Noeud(true,(2,3),[])
let t = Noeud(true,(0,0),[f1;f2;f3])

let _ = let l = getpoints t in let _ = dump l in Printf.printf "%d\n" (weight t)

let t = Noeud(true,(0,0),[f1])
