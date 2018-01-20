type t = V of float * float * float

let create x y z = V (x, y, z)

let abs x = if x < 0. then -.x else x

let add (V (x, y, z)) (V (x', y', z')) = V (x +. x', y +. y', z +. z')

let subtract (V (x, y, z)) (V (x', y', z')) = V (x -. x', y -. y', z -. z')

let displacement v0 v1 = subtract v1 v0

let mult (V (x, y, z)) c = V (x *. c, y *. c, z *. c)

let div (V (x, y, z)) c = V (x /. c, y /. c, z /. c)

let dot_prod (V (x, y, z)) (V (x', y', z')) = x*.x' +. y*.y' +. z*.z'

let dist_sq (V (x, y, z)) (V (x', y', z')) = let dx = x -. x' and dy = y -. y' and dz = z -. z' in dx*.dx +. dy*.dy +. dz*.dz

let dist v0 v1 = sqrt (dist_sq v0 v1)

let len v = dist (V (0., 0., 0.)) v

let normalize v =
    match v with
    V (x, y, z) -> let len = len v in
        V (x /. len, y /. len, z /. len)

let opposite (V (x, y, z)) = V (-.x, -.y, -.z)

let symmetric p normal =
    let normal = normalize normal in
    subtract p (mult normal (2. *. dot_prod p normal))

let angle v0 v1 = acos (dot_prod v0 v1 /. (len v0 *. len v1))

let abs_angle v0 v1 = abs (angle v0 v1)

let test_symmetric () =
    let p = create 0. 1. 0.
    and dir = create 0. 1. 0. in
    symmetric p dir

let test_angle () =
    let v0 = create 0. 0. (-1.)
    and v1 = create 1. 1. (-1.) in
    angle v0 v1
