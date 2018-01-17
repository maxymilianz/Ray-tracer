let solve_quadratic_equation a b c =        (* ax^2 + bx + c = 0 *)
    let delta = b*.b -. 4.*.a*.c in
    if delta < 0. then None
    else Some ((-.b +. sqrt delta) /. (2.*.a), (-.b -. sqrt delta) /. (2.*.a))

module type COLOR = sig
    type t = C of float * float * float        (* (r, g, b) *)

    val black : t
    val white : t
    val red : t
    val green : t
    val blue : t

    val create : float -> float -> float -> t

    val add : t -> t -> t
    val mult : t -> float -> t
    val div : t -> float -> t

    val mix3 : t -> t -> t -> (float * float * float) -> t      (* floats triple <= 0 *)

    val max_int : int

    val to_int : t -> (int * int * int)
    val from_int : int -> int -> int -> t

    val to_graphics_color : t -> int
end

module Color : COLOR = struct
    type t = C of float * float * float

    let black = C (0., 0., 0.)
    let white = C (1., 1., 1.)
    let red = C (1., 0., 0.)
    let green = C (0., 1., 0.)
    let blue = C (0., 0., 1.)

    let create r g b = C (r, g, b)

    let add (C (r, g, b)) (C (r', g', b')) = C (r+.r', g+.g', b+.b')

    let mult (C (r, g, b)) c = C (r*.c, g*.c, b*.c)

    let div (C (r, g, b)) c = C (r/.c, g/.c, b/.c)    

    let mix3 color0 color1 color2 (ratio0, ratio1, ratio2) = add (mult color0 ratio0) (add (mult color1 ratio1) (mult color2 ratio2))

    let max_int = 255

    let to_int c =
        match mult c (float max_int) with
        C (r, g, b) -> int_of_float r, int_of_float g, int_of_float b

    let from_int r g b = div (C (float r, float g, float b)) (float max_int)

    let to_graphics_color c =
        let r, g, b = to_int c in
        (r lsl 16) lor (g lsl 8) lor b
end

module type VECTOR = sig
    type t = V of float * float * float

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
    val symmetric : t -> t -> t -> t        (* point to reflect -> dir anchorage point -> dir -> reflected point *)
end

module Vector : VECTOR = struct
    type t = V of float * float * float

    let create x y z = V (x, y, z)

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

    let symmetric p anchor dir =
        let norm_dir = normalize dir
        and op_p = subtract anchor p in
        add anchor (subtract op_p (mult norm_dir (2. *. dot_prod op_p norm_dir)))
end

let test_vector_symmetric () =
    let p = Vector.create 0. 1. 0.
    and anchor = Vector.create 50. 0. 0.
    and dir = Vector.create 0. 1. 0. in
    Vector.symmetric p anchor dir

module type LIGHT = sig
    type t = Point of Vector.t * float | Sun of Vector.t * float        (* Vector.t in Point is position, in Sun - direction and float is intensity *)

    val max_intensity : float
    val min_intensity : float
   
    val intensity : Vector.t -> t -> float
    val valid_intensity : float -> float
end

module Light : LIGHT = struct
    type t = Point of Vector.t * float | Sun of Vector.t * float

    let max_intensity = 1.

    let min_intensity = 0.1

    let intensity pos = function
        Point (pos', intensity) -> min max_intensity (1. /. Vector.dist_sq pos pos')
        | Sun (_, intensity) -> min max_intensity intensity

    let valid_intensity intensity =
        if intensity < min_intensity then min_intensity
        else if intensity > max_intensity then max_intensity
        else intensity
end

module type SPHERE = sig
    type t = Vector.t * float * Color.t * (float * float * float)       (* center position, radius, color and (glowing, reflecting, scattering) ratio <= 1 *)

    val create : Vector.t -> float -> Color.t -> (float * float * float) -> t
    val intersection : Vector.t -> Vector.t -> t -> Vector.t option     (* camera pos -> camera dir -> sphere -> optional intersection point *)
    val normal : Vector.t -> t -> Vector.t
end

module Sphere : SPHERE = struct
    type t = Vector.t * float * Color.t * (float * float * float)

    let create pos radius color ratio = pos, radius, color, ratio

    let intersection pos dir (center, r, _, _) =
        let l = Vector.displacement center pos in
        let a = Vector.dot_prod dir dir
        and b = 2. *. Vector.dot_prod dir l
        and c = Vector.dot_prod l l -. (r*.r) in
        match solve_quadratic_equation a b c with
        None -> None
        | Some (x0, x1) ->
            if x0 < 0. || x1 < 0. then None
            else let closer = min x0 x1 in
                Some Vector.(add pos (mult dir closer))

    let normal p (center, _, _, _) = Vector.(normalize (displacement center p))
end

let test_sphere_intersection () =
    let open Vector in
    let pos, dir = create 0. 0. 0., create 50. 50. 50.
    and sph : Sphere.t = create 10. 10. 10., 10., Color.black, (1., 2., 3.) in
    Sphere.intersection pos dir sph

module type SURFACE = sig
    type t = Vector.t * Vector.t * Color.t * (float * float * float)      (* normal, point on surface, color and (glowing, reflecting, scattering) ratio <= 1 *)

    val create : Vector.t -> Vector.t -> Color.t -> (float * float * float) -> t
    val intersection : Vector.t -> Vector.t -> t -> Vector.t option     (* camera pos -> camera dir -> surface -> optional intersection point *)
    val normal : t -> Vector.t    
end

module Surface : SURFACE = struct
    type t = Vector.t * Vector.t * Color.t * (float * float * float)

    let create normal point color ratio = normal, point, color, ratio

    let intersection pos dir (normal, point, _, _) =
        let denominator = Vector.dot_prod dir normal in
        if denominator = 0. then None
        else let nominator = Vector.(dot_prod (subtract point pos) normal) in
            let coeff = nominator /. denominator in
            if coeff < 0. then None
            else Some Vector.(add pos (mult dir coeff))
    
    let normal (normal, _, _, _) = normal
end

let test_surface_intersection () =
    let open Vector in
    let pos, dir = create 0. 1. 0., create 2. (-1.) 0.
    and surf : Surface.t = create 0. 1. 0., create 0. 0. 0., Color.black, (1., 1., 1.) in
    Surface.intersection pos dir surf

module type OBJ = sig
    type t = Sph of Sphere.t | Surf of Surface.t

    val intersection : Vector.t -> Vector.t -> t -> Vector.t option     (* camera pos -> camera dir -> obj -> optional point of intersection *)
    val normal : Vector.t -> t -> Vector.t
    
    (* camera pos -> intersection point -> obj -> objs -> lights -> bg color -> remaining recursion depth -> color *)
    val resultant_color : Vector.t -> Vector.t -> t -> t list -> Light.t list -> Color.t -> int -> Color.t
    
    (* camera pos -> camera dir -> objs -> optional intersection point *)
    val closest_intersection : Vector.t -> Vector.t -> t list -> (t * Vector.t) option
end

module Obj : OBJ = struct
    type t = Sph of Sphere.t | Surf of Surface.t
    
    let intersection pos dir = function
        Sph sph -> Sphere.intersection pos dir sph
        | Surf surf -> Surface.intersection pos dir surf

    let normal p = function
        Sph sph -> Sphere.normal p sph
        | Surf surf -> Surface.normal surf

    let color = function
        Sph (_, _, color, _) -> color
        | Surf (_, _, color, _) -> color

    let color_ratio = function
        Sph (_, _, _, color_ratio) -> color_ratio
        | Surf (_, _, _, color_ratio) -> color_ratio
    
    let rec color_reflected pos point obj objs lights bg_color rec_depth =
        let dir = Vector.(displacement point (symmetric pos point (normal point obj))) in
        match closest_intersection point dir objs with
        None -> bg_color
        | Some (obj, point') -> resultant_color point point' obj objs lights bg_color rec_depth

    and color_lighted point obj objs lights =
        let rec intensity light =
            match light with
            Light.Point (pos, intensity) ->
                (match closest_intersection point (Vector.displacement point pos) objs with
                None -> Some (Light.intensity point light)
                | Some (obj, point') ->
                    if Vector.(dist point pos > dist point point') then None
                    else Some (Light.intensity point light))
            | Light.Sun (dir, intensity) ->
                (match closest_intersection point (Vector.opposite dir) objs with
                None -> Some (Light.intensity point light)
                | _ -> None) in
        let rec aux current_intensity = function
            [] -> current_intensity
            | hd :: tl -> match intensity hd with
                None -> aux current_intensity tl
                | Some intensity -> aux (current_intensity +. intensity) tl in
        let final_intensity = Light.valid_intensity (aux 0. lights) in
        Color.mult (color obj) final_intensity

    and resultant_color pos point obj objs lights bg_color rec_depth =
        let glow, refl, scat = color_ratio obj in
        let glowed = if glow = 0. then Color.black else color obj
        and reflected = if refl = 0. || rec_depth = 0 then Color.black else color_reflected pos point obj objs lights bg_color (rec_depth - 1)
        and scattered = if scat = 0. then Color.black else color_lighted point obj objs lights in
        Color.mix3 glowed reflected scattered (glow, refl, scat)

    and closest_intersection pos dir objs =       (* camera pos -> camera dir -> objs -> optional object and point of intersection *)
        let rec aux closest dist = function     (* optional current closest object and point -> dist (not optional; None => infinity) -> same return type as above *)
            [] -> closest
            | hd :: tl -> match intersection pos dir hd with      (* this match checks if ray from camera intersects with obj *)
                None -> aux closest dist tl
                | Some point -> let new_dist = Vector.dist pos point in
                    if new_dist < dist then aux (Some (hd, point)) new_dist tl
                    else aux closest dist tl in
        aux None infinity objs
end

(* let pixel_to_vector res_x res_y x y (ul, ur, lr, ll) =
    let horizontal, vertical = Vector.displacement ul ur, Vector.displacement ul ll in
    let ratio_x, ratio_y = float x /. float res_x, float y /. float res_y in
    Vector.(add ul (add (mult horizontal ratio_x) (mult vertical ratio_y)))

let test_pixel_to_vector () = let open Vector in
    let res_x, res_y = 1000, 1000
    and x, y = 161, 415
    and canvas_coords = create 0. 0. 0., create 1000. 0. 0., create 1000. (-1000.) 0., create 0. (-1000.) 0. in
    pixel_to_vector res_x res_y x y canvas_coords *)

let pixels_to_vectors res_x res_y (ul, ur, lr, ll) =
    let horizontal, vertical = Vector.(displacement ul ur, displacement ul ll) in
    let step_x, step_y = Vector.(div horizontal (float res_x), div vertical (float res_y)) in
    let rec aux_y current_vector y =
        if y = res_y then []
        else let rec aux_x current_vector x =
            if x = res_x then []
            else let pos = Vector.add current_vector step_x in
                pos :: aux_x pos (x + 1) in
            aux_x current_vector 0 :: aux_y (Vector.add current_vector step_y) (y + 1) in
    aux_y ul 0

let render res_x res_y canvas_coords pos objs lights bg_color rec_depth =        (* returns list of lists of colors (res_y * res_x) *)
    let vectors = pixels_to_vectors res_x res_y canvas_coords in
    let rec aux_y vectors y =
        if y = res_y then []
        else match vectors with
            hd :: tl ->
                let rec aux_x vectors x =
                    if x = res_x then []
                    else match vectors with
                        hd :: tl ->
                            let dir = Vector.displacement pos hd in
                            (match Obj.closest_intersection pos dir objs with
                                None -> bg_color
                                | Some (obj, point) -> Obj.resultant_color pos point obj objs lights bg_color rec_depth) :: aux_x tl (x + 1) in
                aux_x hd 0 :: aux_y tl (y + 1) in
    aux_y vectors 0

let pixels_to_array res_x res_y pixels =
    let array = Array.init res_y (fun i -> Array.make res_x (0, 0, 0)) in
    let rec aux_y y = function
        [] -> []
        | hd :: tl ->
            let rec aux_x x = function
                [] -> []
                | hd :: tl -> (array.(y).(x) <- Color.to_int hd) :: aux_x (x + 1) tl in
            aux_x 0 hd :: aux_y (y + 1) tl in
    aux_y 0 (List.rev pixels);
    array

let to_file filename res_x res_y pixels =
    let open Printf in
    let stream = open_out filename in
    fprintf stream "P3\n";
    fprintf stream "%d %d\n" res_x res_y;
    fprintf stream "%d\n" Color.max_int;
    let array = pixels_to_array res_x res_y pixels in
    for y = 0 to res_y - 1 do
        for x = 0 to res_x - 1 do
            let r, g, b = array.(y).(x) in
            fprintf stream "%d %d %d\t\t" r g b
        done;
        fprintf stream "\n"
    done;
    close_out stream

let pixels_to_image res_x res_y pixels =
    let open Graphics in
    let color_array = Array.init res_y (fun i -> Array.make res_x 0) in
    let rec aux_y y = function
        [] -> []
        | hd :: tl ->
            let rec aux_x x = function
                [] -> []
                | hd :: tl -> (color_array.(y).(x) <- Color.to_graphics_color hd) :: aux_x (x + 1) tl in
            aux_x 0 hd :: aux_y (y + 1) tl in
    aux_y 0 (List.rev pixels);
    color_array

let display res_x res_y pixels =
    let open Graphics in
    open_graph (string_of_int res_x ^ "x" ^ string_of_int res_y);
    (* resize_window res_x res_y; *)
    set_window_title "Ray-tracer";
    draw_image (make_image (pixels_to_image res_x res_y pixels)) 0 0

let objs_for_test () =
    let sph = Sphere.create (Vector.create 100. 100. 400.) 50. Color.red (0., 0., 1.)
    and surf = Surface.create (Vector.create 0. 0. (-1.)) (Vector.create 0. 0. 400.) Color.blue (0., 0., 1.) in
    [Obj.Sph sph; Obj.Surf surf]

let lights_for_test () =
    let sun = Light.Sun (Vector.create (-1.) (-1.) 1., 1.) in
    [sun]

let test filename =
    let res_x, res_y = 1280, 720
    and canvas_coords = (Vector.create 0. 720. 0., Vector.create 1280. 720. 0., Vector.create 1280. 0. 0., Vector.create 0. 0. 0.)
    and pos = Vector.create 640. 360. (-500.)
    and objs = objs_for_test ()
    and lights = lights_for_test ()
    and bg_color = Color.black
    and rec_depth = 5 in
    let pixels = render res_x res_y canvas_coords pos objs lights bg_color rec_depth in
    if filename = "" then display res_x res_y pixels
    else to_file filename res_x res_y pixels