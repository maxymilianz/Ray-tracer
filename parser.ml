open Str

let int_regexp = regexp "-?[0-9]+"

let float_regexp = regexp "-?[0-9]+\.[0-9]*"

let sph_regexp, surf_regexp = regexp "Sph", regexp "Surf"

let point_regexp, sun_regexp = regexp "Point", regexp "Sun"

(* string -> pos to start searching -> (found int, new pos to start searching for sth else), functions below similar *)
let match_int str pos =
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
    Myobj.create_sph (Sphere.create center radius color color_ratio), pos4

let match_surf str pos =
    let normal, pos1 = match_vector str pos in
    let point, pos2 = match_vector str pos1 in
    let color, pos3 = match_color str pos2 in
    let color_ratio, pos4 = match_color_ratio str pos3 in
    Myobj.create_surf (Surface.create normal point color color_ratio), pos4

(* returns closer (in the sense of position in .mza file) optional object, choosing from Sph and Surf (in particular they may not exist) *)
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

(* similar to closer_obj, but for Point and Sun Light *)
let closer_light str = function
    None, None -> None
    | None, Some sun_pos -> Some (match_sun_light str sun_pos)
    | Some point_pos, None -> Some (match_point_light str point_pos)
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

(* contents of .mza file -> scene (may throw some exceptions for incompatible file) *)
let str_to_scene str = match_scene str 0

(* filename -> scene (may throw some exceptions for incompatible file) *)
let read_file filename =
    let stream = open_in filename in
    let rec aux str =
        try
            aux (str ^ input_line stream)
        with e ->
            close_in_noerr stream;
            str in
    str_to_scene (aux "")
