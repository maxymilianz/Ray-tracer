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
    val displacement : t -> t -> t      (* displacement between 2 points *)
    val mult : t -> float -> t
    val dist : t -> t -> float
    val len : t -> float
    val normalize : t -> t
end

module Vector : VECTOR = struct
    type t = V of float * float * float

    let create x y z = V (x, y, z)

    let add (V (x, y, z)) (V (x', y', z')) = V (x +. x', y +. y', z +. z')

    let displacement (V (x, y, z)) (V (x', y', z')) = V (x' -. x, y' -. y, z' -. z)

    let mult (V (x, y, z)) c = V (x *. c, y *. c, z *. c)

    let dist (V (x, y, z)) (V (x', y', z')) = let dx = x -. x' and dy = y -. y' and dz = z -. z' in sqrt (dx*.dx +. dy*.dy +. dz*.dz)

    let len v = dist (V (0., 0., 0.)) v

    let normalize v =
        match v with
        V (x, y, z) -> let len = len v in
            V (x /. len, y /. len, z /. len)
end

module type SPHERE = sig
    type t = Vector.t * float * Color.t * (float * float * float)       (* center position, radius, color and (glowing, reflecting, scattering) which should sum up to 1.0 *)

    val intersection : Vector.t -> Vector.t -> t -> Vector.t option
end

module Sphere : SPHERE = struct
    type t = Vector.t * float * Color.t * (float * float * float)

    let intersection q w e = Some q (* TOOD *)
end

module type SURFACE = sig
    type t = (Vector.t * Vector.t * Vector.t * Vector.t) * Color.t * (float * float * float)      (* 4 vertexes, color and (glowing, reflecting, scattering) which should sum up to 1.0 *)

    val intersection : Vector.t -> Vector.t -> t -> Vector.t option
end

module Surface : SURFACE = struct
    type t = (Vector.t * Vector.t * Vector.t * Vector.t) * Color.t * (float * float * float)

    let intersection q w e = Some q (* TOOD *)
end

module type OBJ = sig
    type t = Sph of Sphere.t | Surf of Surface.t

    val intersection : Vector.t -> Vector.t -> t -> Vector.t option     (* camera pos -> camera dir -> obj -> optional point of intersection *)
end

module Obj : OBJ = struct
    type t = Sph of Sphere.t | Surf of Surface.t
    
    let intersection pos dir = function
        Sph s -> Sphere.intersection pos dir s
        | Surf s -> Surface.intersection pos dir s
end

let pixel_to_vector res_x res_y x y canvas_coords =
    (* canvas_coords is tuple of upper left, upper right, lower right and lower left Vectors *)
    match canvas_coords with
    ul, ur, lr, ll -> let horizontal, vertical = Vector.displacement ul ur, Vector.displacement ul ll in
        let width, height = Vector.len horizontal, Vector.len vertical in
        let ratio_x, ratio_y = float x /. float res_x, float y /. float res_y in
        Vector.(add ul (add (mult horizontal ratio_x) (mult vertical ratio_y)))

let test_pixel_to_vector = let open Vector in
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

let color pos point obj = Color.create 1 2 3        (* TODO *)

let render res_x res_y canvas_coords pos objs bg_color =        (* returns list of lists of colors (res_y * res_x) *)
    let rec aux_y y =
        if y = res_y then []
        else let rec aux_x x =
            if x = res_x then []
            else let dir = Vector.displacement pos (pixel_to_vector res_x res_y x y canvas_coords) in
                match closest_intersection pos dir objs with
                    None -> bg_color
                    | Some (obj, point) -> color pos point obj :: aux_x (x + 1) in
            aux_x 0 :: aux_y (y + 1) in
    aux_y 0