module type VECTOR = sig
    type t = V of float * float * float

    val create : float -> float -> float -> t
    val displacement : t -> t -> t
end

module Vector : VECTOR = struct
    type t = V of float * float * float

    let create x y z = V (x, y, z)

    let displacement (V (x, y, z)) (V (x', y', z')) = V (x' -. x, y' -. y, z' -. z)
end

module type OBJ = sig
    (* TODO *)
    val color : int -> int -> int
end

module Obj : OBJ = struct
    (* TODO *)
    let color x y = x + y
end

let pixel_to_vector q w e = Vector.create 1. 2. 3. (* TODO *)

let closest_intersection q w e = Some (1, 2) (* TODO *)

let render res_x res_y canvas_coords pos objects bg_color =
    let rec aux_y y =
        if y = res_y then []
        else let rec aux_x x =
            if x = res_x then []
            else let v = Vector.displacement pos (pixel_to_vector x y canvas_coords) in
                match closest_intersection pos v objects with
                    None -> bg_color
                    | Some (obj, point) -> Obj.color obj point :: aux_x (x + 1) in
            aux_x 0 :: aux_y (y + 1) in
    aux_y 0