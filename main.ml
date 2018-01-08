module type COLOR = sig
    type t = int * int * int        (* (r, g, b) *)

    val create : int -> int -> int -> t
end

module Color : COLOR = struct
    type t = int * int * int

    let create r g b = (r, g, b)
end

module type VECTOR = sig
    type t = V of float * float * float

    val create : float -> float -> float -> t
    val add : t -> t -> t
    val subtract : t -> t -> t
    val displacement : t -> t -> t      (* displacement between 2 points *)
    val mult : t -> float -> t
    val dot_prod : t -> t -> float
    val dist : t -> t -> float
    val len : t -> float
    val normalize : t -> t
    val symmetric : t -> t -> t
end

module Vector : VECTOR = struct
    type t = V of float * float * float

    let create x y z = V (x, y, z)

    let add (V (x, y, z)) (V (x', y', z')) = V (x +. x', y +. y', z +. z')

    let subtract (V (x, y, z)) (V (x', y', z')) = V (x -. x', y -. y', z -. z')    

    let displacement v0 v1 = subtract v1 v0

    let mult (V (x, y, z)) c = V (x *. c, y *. c, z *. c)

    let dot_prod (V (x, y, z)) (V (x', y', z')) = x*.x' +. y*.y' +. z*.z'

    let dist (V (x, y, z)) (V (x', y', z')) = let dx = x -. x' and dy = y -. y' and dz = z -. z' in sqrt (dx*.dx +. dy*.dy +. dz*.dz)

    let len v = dist (V (0., 0., 0.)) v

    let normalize v =
        match v with
        V (x, y, z) -> let len = len v in
            V (x /. len, y /. len, z /. len)

    let symmetric v ref =
        let ref_norm = normalize ref in
        subtract v (mult ref_norm (2. *. dot_prod v ref_norm))
end

let test_vector_symmetric () =
    let v = Vector.create 0. 1. 0.
    and ref = Vector.create (-1.) (-1.) 0. in
    Vector. symmetric v ref

let solve_quadratic_equation a b c =
    let delta = b*.b -. 4.*.a*.c in
    if delta < 0. then None
    else Some ((-.b +. sqrt delta) /. (2.*.a), (-.b -. sqrt delta) /. (2.*.a))

let min a b = if a < b then a else b

module type SPHERE = sig
    type t = Vector.t * float * Color.t * (float * float * float)       (* center position, radius, color and (glowing, reflecting, scattering) which should sum up to 1.0 *)

    val intersection : Vector.t -> Vector.t -> t -> Vector.t option
    val normal : Vector.t -> t -> Vector.t
end

module Sphere : SPHERE = struct
    type t = Vector.t * float * Color.t * (float * float * float)

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
end

let test_sphere_intersection () =
    let open Vector in
    let pos, dir = create 0. 0. 0., create 50. 50. 50.
    and sph : Sphere.t = create 10. 10. 10., 10., Color.create 1 2 3, (1., 2., 3.) in
    Sphere.intersection pos dir sph

module type SURFACE = sig
    type t = (Vector.t * Vector.t * Vector.t * Vector.t) * Color.t * (float * float * float)      (* 4 vertexes, color and (glowing, reflecting, scattering) which should sum up to 1.0 *)

    val intersection : Vector.t -> Vector.t -> t -> Vector.t option
    val normal : Vector.t -> t -> Vector.t    
end

module Surface : SURFACE = struct
    type t = (Vector.t * Vector.t * Vector.t * Vector.t) * Color.t * (float * float * float)

    let intersection q w e = Some q (* TOOD *)
    
    let normal p ((ul, ur, lr, ll), _, _) = p (* TODO *)
end

module type OBJ = sig
    type t = Sph of Sphere.t | Surf of Surface.t

    val intersection : Vector.t -> Vector.t -> t -> Vector.t option     (* camera pos -> camera dir -> obj -> optional point of intersection *)
    val normal : Vector.t -> t -> Vector.t
end

module Obj : OBJ = struct
    type t = Sph of Sphere.t | Surf of Surface.t
    
    let intersection pos dir = function
        Sph sph -> Sphere.intersection pos dir sph
        | Surf surf -> Surface.intersection pos dir surf

    let normal p = function
        Sph sph -> Sphere.normal p sph
        | Surf surf -> Surface.normal p surf
end

let pixel_to_vector res_x res_y x y canvas_coords =
    (* canvas_coords is tuple of upper left, upper right, lower right and lower left Vectors *)
    match canvas_coords with
    ul, ur, lr, ll -> let horizontal, vertical = Vector.displacement ul ur, Vector.displacement ul ll in
        let ratio_x, ratio_y = float x /. float res_x, float y /. float res_y in
        Vector.(add ul (add (mult horizontal ratio_x) (mult vertical ratio_y)))

let test_pixel_to_vector () = let open Vector in
    let res_x, res_y = 1000, 1000
    and x, y = 161, 415
    and canvas_coords = create 0. 0. 0., create 1000. 0. 0., create 1000. (-1000.) 0., create 0. (-1000.) 0. in
    pixel_to_vector res_x res_y x y canvas_coords

let closest_intersection pos dir objs =       (* camera pos -> camera dir -> objs -> optional object and point of intersection *)
    let rec aux closest dist = function     (* optional current closest object and point -> dist (not optional; None => infinity) -> same return type as above *)
        [] -> closest
        | hd :: tl -> match Obj.intersection pos dir hd with      (* this match checks if ray from camera intersects with obj *)
            None -> aux closest dist tl
            | Some point -> let new_dist = Vector.dist pos point in
                if new_dist < dist then aux (Some (hd, point)) new_dist tl
                else aux closest dist tl in
    aux None infinity objs

let color pos point obj objs = Color.create 1 2 3        (* TODO *)

let render res_x res_y canvas_coords pos objs bg_color =        (* returns list of lists of colors (res_y * res_x) *)
    let rec aux_y y =
        if y = res_y then []
        else let rec aux_x x =
            if x = res_x then []
            else let dir = Vector.displacement pos (pixel_to_vector res_x res_y x y canvas_coords) in
                match closest_intersection pos dir objs with
                    None -> bg_color
                    | Some (obj, point) -> color pos point obj objs :: aux_x (x + 1) in
            aux_x 0 :: aux_y (y + 1) in
    aux_y 0