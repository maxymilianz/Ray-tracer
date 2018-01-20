type t

val create : float -> float -> float -> t

val add : t -> t -> t
val subtract : t -> t -> t
val displacement : t -> t -> t
val mult : t -> float -> t
val div : t -> float -> t

val dot_prod : t -> t -> float

val dist_sq : t -> t -> float
val dist : t -> t -> float
val len : t -> float

val normalize : t -> t
val opposite : t -> t
val symmetric : t -> t -> t        (* point to reflect -> normal -> reflected point *)

val angle : t -> t -> float
val abs_angle : t -> t -> float
