type t

val create : Vector.t -> Vector.t -> Color.t -> (float * float * float) -> t
val intersection : Vector.t -> Vector.t -> t -> Vector.t option     (* camera pos -> camera dir -> surface -> optional intersection point *)
val normal : t -> Vector.t
val color : t -> Color.t
val color_ratio : t -> (float * float * float)
