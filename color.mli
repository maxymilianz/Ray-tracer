type t

val black : t
val white : t
val red : t
val green : t
val blue : t
val grey : t

val create : float -> float -> float -> t

val mult : t -> float -> t
val div : t -> float -> t

val mix3 : t -> t -> t -> (float * float * float) -> t      (* floats triple <= 0 *)

val max_int : int

val to_int : t -> (int * int * int)
val from_int : int -> int -> int -> t

val to_graphics_color : t -> int
