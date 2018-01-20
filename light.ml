type t = Point of Vector.t * float | Sun of Vector.t * float        (* Vector.t in Point is position, in Sun - direction and float is intensity *)

let create_point pos intensity = Point (pos, intensity)

let create_sun dir intensity = Sun (dir, intensity)

let max_intensity = 1.

let min_intensity = 0.4

let intensity pos = function
    Point (pos', intensity) -> min max_intensity (1. /. Vector.dist_sq pos pos')
    | Sun (_, intensity) -> min max_intensity intensity

let valid_intensity intensity =
    if intensity < min_intensity then min_intensity
    else if intensity > max_intensity then max_intensity
    else intensity
