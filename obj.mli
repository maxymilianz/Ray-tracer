type t = Sph of Sphere.t | Surf of Surface.t

val create_sph : Sphere.t -> t
val create_surf : Surface.t -> t

(* camera pos -> intersection point -> obj -> objs -> lights -> bg color -> remaining recursion depth -> color *)
val resultant_color : Vector.t -> Vector.t -> t -> t list -> Light.t list -> Color.t -> int -> Color.t

(* camera pos -> camera dir -> objs -> optional intersection point *)
val closest_intersection : Vector.t -> Vector.t -> t list -> (t * Vector.t) option
