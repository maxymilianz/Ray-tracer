module type VECTOR = sig
    type t = float * float * float

    val create : float -> float -> float -> t
    val displacement : t -> t -> t
end

module Vector : VECTOR = struct
    type t = float * float * float

    let create x y z = (x, y, z)

    let displacement (x, y, z) (x', y', z') = (x' - x, y' - y, z' - z)
end