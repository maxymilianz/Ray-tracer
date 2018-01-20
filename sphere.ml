type t = Vector.t * float * Color.t * (float * float * float)       (* center position, radius, color and (glowing, reflecting, scattering) ratio <= 1 *)

let create pos radius color ratio = pos, radius, color, ratio

let solve_quadratic_equation a b c =        (* ax^2 + bx + c = 0 *)
    let delta = b*.b -. 4.*.a*.c in
    if delta < 0. then None
    else Some ((-.b +. sqrt delta) /. (2.*.a), (-.b -. sqrt delta) /. (2.*.a))

let intersection pos dir (center, r, _, _) =
    let l = Vector.displacement center pos in
    let a = Vector.dot_prod dir dir
    and b = 2. *. Vector.dot_prod dir l
    and c = Vector.dot_prod l l -. (r*.r) in
    match solve_quadratic_equation a b c with
    None -> None
    | Some (x0, x1) ->
        if x0 < 0. || x1 < 0. then None
        else let closer = min x0 x1 in
            Some Vector.(add pos (mult dir closer))

let normal p (center, _, _, _) = Vector.(normalize (displacement center p))

let color (_, _, color, _) = color

let color_ratio (_, _, _, color_ratio) = color_ratio

let test_intersection () =
    let open Vector in
    let pos, dir = create 0. 0. 0., create 50. 50. 50.
    and sph = create (create 10. 10. 10.) 10. Color.black (1., 2., 3.) in
    intersection pos dir sph
