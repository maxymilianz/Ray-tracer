let pi = 4.0 *. atan 1.0

let solve_quadratic_equation a b c =        (* ax^2 + bx + c = 0 *)
    let delta = b*.b -. 4.*.a*.c in
    if delta < 0. then None
    else Some ((-.b +. sqrt delta) /. (2.*.a), (-.b -. sqrt delta) /. (2.*.a))

let abs x = if x < 0. then -.x else x

module type COLOR = sig
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
end

module Color : COLOR = struct
    type t = C of float * float * float        (* (r, g, b) *)

    let black = C (0., 0., 0.)
    let white = C (1., 1., 1.)
    let red = C (1., 0., 0.)
    let green = C (0., 1., 0.)
    let blue = C (0., 0., 1.)
    let grey = C (0.5, 0.5, 0.5)

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

    let symmetric p normal =
        let normal = normalize normal in
        subtract p (mult normal (2. *. dot_prod p normal))

    let angle v0 v1 = acos (dot_prod v0 v1 /. (len v0 *. len v1))

    let abs_angle v0 v1 = abs (angle v0 v1)
end

let test_vector_symmetric () =
    let p = Vector.create 0. 1. 0.
    and dir = Vector.create 0. 1. 0. in
    Vector.symmetric p dir

let test_vector_angle () =
    let v0 = Vector.create 0. 0. (-1.)
    and v1 = Vector.create 1. 1. (-1.) in
    Vector.angle v0 v1

module type LIGHT = sig
    type t = Point of Vector.t * float | Sun of Vector.t * float

    val create_point : Vector.t -> float -> t
    val create_sun : Vector.t -> float -> t

    val max_intensity : float
    val min_intensity : float
   
    val intensity : Vector.t -> t -> float
    val valid_intensity : float -> float
end

module Light : LIGHT = struct
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
end

module type SPHERE = sig
    type t

    val create : Vector.t -> float -> Color.t -> (float * float * float) -> t
    val intersection : Vector.t -> Vector.t -> t -> Vector.t option     (* camera pos -> camera dir -> sphere -> optional intersection point *)
    val normal : Vector.t -> t -> Vector.t
    val color : t -> Color.t
    val color_ratio : t -> (float * float * float)
end

module Sphere : SPHERE = struct
    type t = Vector.t * float * Color.t * (float * float * float)       (* center position, radius, color and (glowing, reflecting, scattering) ratio <= 1 *)

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

    let color (_, _, color, _) = color

    let color_ratio (_, _, _, color_ratio) = color_ratio
end

let test_sphere_intersection () =
    let open Vector in
    let pos, dir = create 0. 0. 0., create 50. 50. 50.
    and sph = Sphere.create (create 10. 10. 10.) 10. Color.black (1., 2., 3.) in
    Sphere.intersection pos dir sph

module type SURFACE = sig
    type t

    val create : Vector.t -> Vector.t -> Color.t -> (float * float * float) -> t
    val intersection : Vector.t -> Vector.t -> t -> Vector.t option     (* camera pos -> camera dir -> surface -> optional intersection point *)
    val normal : t -> Vector.t
    val color : t -> Color.t
    val color_ratio : t -> (float * float * float)
end

module Surface : SURFACE = struct
    type t = Vector.t * Vector.t * Color.t * (float * float * float)      (* normal, point on surface, color and (glowing, reflecting, scattering) ratio <= 1 *)

    let create normal point color ratio = normal, point, color, ratio

    let intersection pos dir (normal, point, _, _) =
        let denominator = Vector.dot_prod dir normal in
        if denominator = 0. then None
        else let nominator = Vector.(dot_prod (subtract point pos) normal) in
            let coeff = nominator /. denominator in
            if coeff < 0. then None
            else Some Vector.(add pos (mult dir coeff))
    
    let normal (normal, _, _, _) = Vector.normalize normal

    let color (_, _, color, _) = color

    let color_ratio (_, _, _, color_ratio) = color_ratio
end

let test_surface_intersection () =
    let open Vector in
    let pos, dir = create 0. 1. 0., create 2. (-1.) 0.
    and surf = Surface.create (create 0. 1. 0.) (create 0. 0. 0.) Color.black (1., 1., 1.) in
    Surface.intersection pos dir surf

module type OBJ = sig
    type t = Sph of Sphere.t | Surf of Surface.t

    val create_sph : Sphere.t -> t
    val create_surf : Surface.t -> t

    (* camera pos -> intersection point -> obj -> objs -> lights -> bg color -> remaining recursion depth -> color *)
    val resultant_color : Vector.t -> Vector.t -> t -> t list -> Light.t list -> Color.t -> int -> Color.t
    
    (* camera pos -> camera dir -> objs -> optional intersection point *)
    val closest_intersection : Vector.t -> Vector.t -> t list -> (t * Vector.t) option
end

module Obj : OBJ = struct
    type t = Sph of Sphere.t | Surf of Surface.t

    let create_sph sph = Sph sph

    let create_surf surf = Surf surf
    
    let intersection pos dir = function
        Sph sph -> Sphere.intersection pos dir sph
        | Surf surf -> Surface.intersection pos dir surf

    let normal p = function
        Sph sph -> Sphere.normal p sph
        | Surf surf -> Surface.normal surf

    let color = function
        Sph sph -> Sphere.color sph
        | Surf surf -> Surface.color surf

    let color_ratio = function
        Sph sph -> Sphere.color_ratio sph
        | Surf surf -> Surface.color_ratio surf
    
    let rec color_reflected pos point obj objs lights bg_color rec_depth =
        let dir = Vector.(symmetric (displacement pos point) (normal point obj)) in
        match closest_intersection point dir (List.filter (fun x -> x != obj) objs) with
        None -> bg_color
        | Some (obj, point') -> resultant_color point point' obj objs lights bg_color rec_depth

    and color_lighted point obj objs lights =
        let intensity light =
            let normal = normal point obj in
            match light with
            Light.Point (pos, intensity) ->
                let dir = Vector.displacement point pos in
                if Vector.abs_angle normal dir > pi /. 2. then None
                else (match closest_intersection point dir (List.filter (fun x -> x != obj) objs) with
                    None -> Some (Light.intensity point light)
                    | Some (obj, point') ->
                        if Vector.(dist point pos > dist point point') then None
                        else Some (Light.intensity point light))
            | Light.Sun (light_dir, intensity) ->
                let rev_light_dir = Vector.opposite light_dir in
                if Vector.abs_angle normal rev_light_dir > pi /. 2. then None
                else (match closest_intersection point rev_light_dir (List.filter (fun x -> x != obj) objs) with
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
        if rec_depth = 0 then
            let change_ratio = (glow+.refl+.scat) /. (glow+.scat) in
            let glow, refl, scat = glow*.change_ratio, 0., scat*.change_ratio in
            Color.mix3 glowed reflected scattered (glow, refl, scat)
        else
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

module type SCENE = sig
    type t = (int * int) * (Vector.t * Vector.t * Vector.t * Vector.t) * Vector.t * Color.t * int * (Obj.t list) * (Light.t list)

    val create : (int * int) -> (Vector.t * Vector.t * Vector.t * Vector.t) -> Vector.t -> Color.t -> int -> (Obj.t list) -> (Light.t list) -> t
    val render : t -> Color.t list list
end

module Scene : SCENE = struct
    type t = (int * int) * (Vector.t * Vector.t * Vector.t * Vector.t) * Vector.t * Color.t * int * (Obj.t list) * (Light.t list)

    let create res canvas_coords camera_pos bg_color rec_depth objs lights = res, canvas_coords, camera_pos, bg_color, rec_depth, objs, lights

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

    let render (res, canvas_coords, pos, bg_color, rec_depth, objs, lights) =        (* returns list of lists of colors (res_y * res_x) *)
        match res with res_x, res_y ->
            let vectors = pixels_to_vectors res_x res_y canvas_coords in
            let rec aux_y = function
                [] -> []
                | hd :: tl ->
                    let rec aux_x = function
                        [] -> []
                        | hd :: tl ->
                            let dir = Vector.displacement pos hd in
                            (match Obj.closest_intersection pos dir objs with
                            None -> bg_color
                            | Some (obj, point) -> Obj.resultant_color pos point obj objs lights bg_color rec_depth) :: aux_x tl in
                    aux_x hd :: aux_y tl in
            aux_y vectors
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

let pixels_to_array res_x res_y pixels =
    let array = Array.init res_y (fun i -> Array.make res_x (0, 0, 0)) in
    let rec aux_y y = function
        [] -> ()
        | hd :: tl ->
            let rec aux_x x = function
                [] -> ()
                | hd :: tl -> (array.(y).(x) <- Color.to_int hd); aux_x (x + 1) tl in
            aux_x 0 hd; aux_y (y + 1) tl in
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
        [] -> ()
        | hd :: tl ->
            let rec aux_x x = function
                [] -> ()
                | hd :: tl -> (color_array.(y).(x) <- Color.to_graphics_color hd); aux_x (x + 1) tl in
            aux_x 0 hd; aux_y (y + 1) tl in
    aux_y 0 (List.rev pixels);
    color_array

let display res_x res_y pixels =
    let open Graphics in
    open_graph (string_of_int res_x ^ "x" ^ string_of_int res_y);
    (* resize_window res_x res_y; *)
    set_window_title "Ray-tracer";
    draw_image (make_image (pixels_to_image res_x res_y pixels)) 0 0

let objs_for_test () =
    let sph = Sphere.create (Vector.create (-50.) (-50.) (-50.)) 70. Color.white (1., 0., 0.)
    and sph1 = Sphere.create (Vector.create 0. 620. 200.) 100. Color.red (0., 0.6, 0.4)
    and sph2 = Sphere.create (Vector.create 400. 520. 200.) 200. Color.green (0., 0.6, 0.4)
    and sph3 = Sphere.create (Vector.create 600. 420. 800.) 300. Color.blue (0., 0.6, 0.4)
    and sph4 = Sphere.create (Vector.create 1200. 320. 500.) 400. Color.green (0., 0.6, 0.4)
    and sph5 = Sphere.create (Vector.create (-50.) 420. 500.) 300. Color.blue (0., 0.6, 0.4)
    and sph6 = Sphere.create (Vector.create 800. 570. 100.) 150. Color.red (0., 0.6, 0.4)
    (* and surf = Surface.create (Vector.create 0. 0. (-1.)) (Vector.create 0. 0. 800.) Color.blue (0., 0., 1.) in *)
    and surf = Surface.create (Vector.create 0. (-1.) 0.) (Vector.create 0. 720. 0.) Color.grey (0., 0.6, 0.4) in
    [Obj.Sph sph; Obj.Sph sph1; Obj.Sph sph2; Obj.Sph sph3; Obj.Sph sph4; Obj.Sph sph5; Obj.Sph sph6; Obj.Surf surf]

let lights_for_test () =
    let sun = Light.Sun (Vector.create (1.) (1.) 1., 1.) in
    (* let point = Light.Point (Vector.create 1280. 0. 0., 1.) in *)
    [sun]

let test filename =
    let res_x, res_y = 1280, 720
    and canvas_coords = Vector.create 0. 720. 0., Vector.create 1280. 720. 0., Vector.create 1280. 0. 0., Vector.create 0. 0. 0.
    and pos = Vector.create 640. 360. (-500.)
    and objs = objs_for_test ()
    and lights = lights_for_test ()
    and bg_color = Color.black
    and rec_depth = 4 in
    let pixels = Scene.render (Scene.create (res_x, res_y) canvas_coords pos bg_color rec_depth objs lights) in
    if filename = "" then display res_x res_y pixels
    else to_file filename res_x res_y pixels

module type PARSER = sig
    type t

    val read_file : string -> Scene.t       (* filename -> scene read from file *)
end

module Parser : PARSER = struct
    open Str

    type t = Token_Int of int | Token_Float of float | Token_Vector of Vector.t | Token_Color of Color.t | Token_Sph of Sphere.t | Token_Surf of Surface.t
        | Token_Obj of Obj.t | Token_Light of Light.t

    let int_regexp = regexp "-?[0-9]+"

    let float_regexp = regexp "-?[0-9]+\.[0-9]*"

    let sph_regexp, surf_regexp = regexp "Sph", regexp "Surf"

    let point_regexp, sun_regexp = regexp "Point", regexp "Sun"

    let match_int str pos =     (* string -> pos to start searching -> (found int, new pos to start searching for sth else) *)
        let int_str = search_forward int_regexp str pos; matched_string str in
        int_of_string int_str, match_end ()

    let match_float str pos =
        let float_str = search_forward float_regexp str pos; matched_string str in
        float_of_string float_str, match_end ()

    let match_res str pos =
        let res_x, pos1 = match_int str pos in
        let res_y, pos2 = match_int str pos1 in
        (res_x, res_y), pos2

    let match_vector str pos =
        let x, pos1 = match_float str pos in
        let y, pos2 = match_float str pos1 in
        let z, pos3 = match_float str pos2 in
        Vector.create x y z, pos3

    let match_canvas_coords str pos =
        let ul, pos1 = match_vector str pos in
        let ur, pos2 = match_vector str pos1 in
        let lr, pos3 = match_vector str pos2 in
        let ll, pos4 = match_vector str pos3 in
        (ul, ur, lr, ll), pos4

    let match_color str pos =
        let r, pos1 = match_int str pos in
        let g, pos2 = match_int str pos1 in
        let b, pos3 = match_int str pos2 in
        Color.from_int r g b, pos3

    let match_color_ratio str pos =
        let glow, pos1 = match_float str pos in
        let refl, pos2 = match_float str pos1 in
        let scat, pos3 = match_float str pos2 in
        (glow, refl, scat), pos3

    let match_camera_pos = match_vector

    let match_rec_depth = match_int

    let match_sph str pos =
        let center, pos1 = match_vector str pos in
        let radius, pos2 = match_float str pos1 in
        let color, pos3 = match_color str pos2 in
        let color_ratio, pos4 = match_color_ratio str pos3 in
        Obj.create_sph (Sphere.create center radius color color_ratio), pos4

    let match_surf str pos =
        let normal, pos1 = match_vector str pos in
        let point, pos2 = match_vector str pos1 in
        let color, pos3 = match_color str pos2 in
        let color_ratio, pos4 = match_color_ratio str pos3 in
        Obj.create_surf (Surface.create normal point color color_ratio), pos4

    let closer_obj str = function
        None, None -> None
        | None, Some surf_pos -> Some (match_surf str surf_pos)
        | Some sph_pos, None -> Some (match_sph str sph_pos)
        | Some sph_pos, Some surf_pos ->
            if sph_pos < surf_pos then Some (match_sph str sph_pos)
            else Some (match_surf str surf_pos)

    let match_obj str pos =
        let sph_pos = try
                Some (search_forward sph_regexp str pos)
            with e ->
                None
        and surf_pos = try
                Some (search_forward surf_regexp str pos)
            with e ->
                None in
        closer_obj str (sph_pos, surf_pos)

    let match_objs str pos =
        let rec aux objs pos' =
            match match_obj str pos' with
            None -> objs, pos'
            | Some (obj, pos'') -> aux (obj :: objs) pos'' in
        aux [] pos

    let match_point_light str pos =
        let light_pos, pos1 = match_vector str pos in
        let intensity, pos2 = match_float str pos1 in
        Light.create_point light_pos intensity, pos2

    let match_sun_light str pos =
        let dir, pos1 = match_vector str pos in
        let intensity, pos2 = match_float str pos1 in
        Light.create_sun dir intensity, pos2

    let closer_light str = function
        None, None -> None
        | None, Some sun_pos -> Some (match_point_light str sun_pos)
        | Some point_pos, None -> Some (match_sun_light str point_pos)
        | Some point_pos, Some sun_pos ->
            if point_pos < sun_pos then Some (match_point_light str point_pos)
            else Some (match_sun_light str sun_pos)

    let match_light str pos =
        let point_pos = try
                Some (search_forward point_regexp str pos)
            with e ->
                None
        and sun_pos = try
                Some (search_forward sun_regexp str pos)
            with e ->
                None in
        closer_light str (point_pos, sun_pos)

    let match_lights str pos =
        let rec aux lights pos' =
            match match_light str pos' with
            None -> lights, pos'
            | Some (light, pos'') -> aux (light :: lights) pos'' in
        aux [] pos

    let match_scene str pos =
        let res, pos1 = match_res str pos in
        let canvas_coords, pos2 = match_canvas_coords str pos1 in
        let camera_pos, pos3 = match_camera_pos str pos2 in
        let bg_color, pos4 = match_color str pos3 in
        let rec_depth, pos5 = match_rec_depth str pos4 in
        let objs, pos6 = match_objs str pos5 in
        let lights, _ = match_lights str pos6 in
        Scene.create res canvas_coords camera_pos bg_color rec_depth objs lights

    let str_to_scene str = match_scene str 0

    let read_file filename =
        let stream = open_in filename in
        let rec aux str =
            try
                aux (str ^ input_line stream)
            with e ->
                close_in_noerr stream;
                str in
        str_to_scene (aux "")
end