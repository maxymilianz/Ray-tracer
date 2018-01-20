type t = Vector.t * Vector.t * Color.t * (float * float * float)      (* normal, point on surface, color and (glowing, reflecting, scattering) ratio <= 1 *)

let create normal point color ratio = normal, point, color, ratio

(* following functions are similar to their equivalents in Myobj module *)
let intersection pos dir (normal, point, _, _) =
    let denominator = Vector.dot_prod dir normal in
    if denominator = 0. then None
    else let nominator = Vector.(dot_prod (subtract point pos) normal) in
        let coeff = nominator /. denominator in
        if coeff < 0. then None
        else Some Vector.(add pos (mult dir coeff))

let normal (normal, _, _, _) = Vector.normalize normal

let color (_, _, color, _) = color

let color_ratio (_, _, _, color_ratio) = color_ratio

let test_intersection () =
    let pos, dir = Vector.create 0. 1. 0., Vector.create 2. (-1.) 0.
    and surf = create (Vector.create 0. 1. 0.) (Vector.create 0. 0. 0.) Color.black (1., 1., 1.) in
    intersection pos dir surf
