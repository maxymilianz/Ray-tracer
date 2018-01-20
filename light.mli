type t = Point of Vector.t * float | Sun of Vector.t * float

val create_point : Vector.t -> float -> t
val create_sun : Vector.t -> float -> t

val max_intensity : float
val min_intensity : float

val intensity : Vector.t -> t -> float
val valid_intensity : float -> float
