type t = C of float * float * float        (* (r, g, b), each <= 1 *)

let black = C (0., 0., 0.)
let white = C (1., 1., 1.)
let red = C (1., 0., 0.)
let green = C (0., 1., 0.)
let blue = C (0., 0., 1.)
let grey = C (0.5, 0.5, 0.5)

let create r g b = C (r, g, b)

let add (C (r, g, b)) (C (r', g', b')) = C (r+.r', g+.g', b+.b')

let mult (C (r, g, b)) c = C (r*.c, g*.c, b*.c)     (* multiple each color component with a float *)

let div (C (r, g, b)) c = C (r/.c, g/.c, b/.c)      (* divide each color component by float *)

(* mix 3 colors in given ratio *)
let mix3 color0 color1 color2 (ratio0, ratio1, ratio2) = add (mult color0 ratio0) (add (mult color1 ratio1) (mult color2 ratio2))

let max_int = 255       (* max int to represent color *)

let to_int c =
    match mult c (float max_int) with
    C (r, g, b) -> int_of_float r, int_of_float g, int_of_float b

let from_int r g b = div (C (float r, float g, float b)) (float max_int)

(* to color used by Graphics module *)
let to_graphics_color c =
    let r, g, b = to_int c in
    (r lsl 16) lor (g lsl 8) lor b
