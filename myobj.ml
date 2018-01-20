type t = Sph of Sphere.t | Surf of Surface.t

let create_sph sph = Sph sph

let create_surf surf = Surf surf

(* camera pos -> camera dir -> t -> optional point of intersection *)
let intersection pos dir = function
    Sph sph -> Sphere.intersection pos dir sph
    | Surf surf -> Surface.intersection pos dir surf

(* normal to the object at given point *)
let normal p = function
    Sph sph -> Sphere.normal p sph
    | Surf surf -> Surface.normal surf

let color = function
    Sph sph -> Sphere.color sph
    | Surf surf -> Surface.color surf

(* glow, refl, scat *)
let color_ratio = function
    Sph sph -> Sphere.color_ratio sph
    | Surf surf -> Surface.color_ratio surf

let pi = 4.0 *. atan 1.0

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
