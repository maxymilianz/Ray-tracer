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
    val displacement : t -> t -> t      (* displacement between 2 points *)
    val dist : t -> t -> float
end

module Vector : VECTOR = struct
    type t = V of float * float * float

    let create x y z = V (x, y, z)

    let displacement (V (x, y, z)) (V (x', y', z')) = V (x' -. x, y' -. y, z' -. z)

    let dist (V (x, y, z)) (V (x', y', z')) = let dx = x -. x' and dy = y -. y' and dz = z -. z' in sqrt (dx*.dx +. dy*.dy +. dz*.dz)
end

module type OBJ = sig
    (* TODO *)
    type t = int
    val color : Vector.t -> Vector.t -> t -> Color.t        (* camera pos -> intersection point -> obj -> color visible from camera pos *)
    val intersection : Vector.t -> Vector.t -> t -> Vector.t option     (* camera pos -> camera dir -> obj -> optional point of intersection *)
end

module Obj : OBJ = struct
    (* TODO *)
    type t = int
    let color x y z = Color.create 1 2 3
    let intersection q w e = Some q
end

let pixel_to_vector q w e = Vector.create 1. 2. 3. (* TODO *)

let closest_intersection pos v objs =       (* camera pos -> camera dir -> objs -> optional object and point of intersection *)
    let rec aux closest dist = function     (* optional current closest object and point -> dist (not optional; None => infinity) -> same return type as above *)
        [] -> closest
        | hd :: tl -> match Obj.intersection pos v hd with      (* this match checks if ray from camera intersects with obj *)
            None -> aux closest dist tl
            | Some point -> let new_dist = Vector.dist pos point in
                if new_dist < dist then aux (Some (hd, point)) new_dist tl
                else aux closest dist tl in
    aux None infinity objs

let render res_x res_y canvas_coords pos objs bg_color =        (* returns list of lists of colors (res_y * res_x) *)
    let rec aux_y y =
        if y = res_y then []
        else let rec aux_x x =
            if x = res_x then []
            else let v = Vector.displacement pos (pixel_to_vector x y canvas_coords) in
                match closest_intersection pos v objs with
                    None -> bg_color
                    | Some (obj, point) -> Obj.color pos point obj :: aux_x (x + 1) in
            aux_x 0 :: aux_y (y + 1) in
    aux_y 0