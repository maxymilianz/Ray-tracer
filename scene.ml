(* what t means can be seen in create function below *)
type t = (int * int) * (Vector.t * Vector.t * Vector.t * Vector.t) * Vector.t * Color.t * int * (Myobj.t list) * (Light.t list)

let create res canvas_coords camera_pos bg_color rec_depth objs lights = res, canvas_coords, camera_pos, bg_color, rec_depth, objs, lights

let res (res, _, _, _, _, _, _) = res

(* creates Vector.t list list (vectors from camera pos to each pixel in given resolution and canvas coordinates) *)
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

(* t -> Color.t list list *)
let render ((res_x, res_y), canvas_coords, pos, bg_color, rec_depth, objs, lights) =        (* returns list of lists of colors (res_y * res_x) *)
    let vectors = pixels_to_vectors res_x res_y canvas_coords in
    let rec aux_y = function
        [] -> []
        | hd :: tl ->
            let rec aux_x = function
                [] -> []
                | hd :: tl ->
                    let dir = Vector.displacement pos hd in
                    (match Myobj.closest_intersection pos dir objs with
                    None -> bg_color
                    | Some (obj, point) -> Myobj.resultant_color pos point obj objs lights bg_color rec_depth) :: aux_x tl in
            aux_x hd :: aux_y tl in
    aux_y vectors
