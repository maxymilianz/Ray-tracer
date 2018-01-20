type t

val create : Vector.t -> float -> Color.t -> (float * float * float) -> t
val intersection : Vector.t -> Vector.t -> t -> Vector.t option     (* camera pos -> camera dir -> sphere -> optional intersection point *)
val normal : Vector.t -> t -> Vector.t
val color : t -> Color.t
val color_ratio : t -> (float * float * float)
