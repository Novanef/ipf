open Display

let read_coords =
    let rec aux acc n =
        if n = 0
        then List.rev acc
        else let c = Scanf.scanf "%d %d\n" (fun x y -> (x,y))
             in aux (c::acc) (n-1)
    in
    fun n -> aux [] n

let _ =
    let l = Scanf.scanf "%d\n" read_coords in
    draw_eulcidean (800,600) l [((0.,0.),(2.5,1.5)); ((3.,4.),(2.5,1.5)); ((4.,1.),(2.5,1.5))]