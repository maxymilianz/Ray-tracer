type t = (int * int) * (Vector.t * Vector.t * Vector.t * Vector.t) * Vector.t * Color.t * int * (Obj.t list) * (Light.t list)

val create : (int * int) -> (Vector.t * Vector.t * Vector.t * Vector.t) -> Vector.t -> Color.t -> int -> (Obj.t list) -> (Light.t list) -> t
val res : t -> (int * int)
val render : t -> Color.t list list
