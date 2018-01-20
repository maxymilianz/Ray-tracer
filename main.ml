let pixels_to_array (res_x, res_y) pixels =
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

let to_file filename (res_x, res_y) pixels =
    let open Printf in
    let stream = open_out filename in
    fprintf stream "P3\n";
    fprintf stream "%d %d\n" res_x res_y;
    fprintf stream "%d\n" Color.max_int;
    let array = pixels_to_array (res_x, res_y) pixels in
    for y = 0 to res_y - 1 do
        for x = 0 to res_x - 1 do
            let r, g, b = array.(y).(x) in
            fprintf stream "%d %d %d\t\t" r g b
        done;
        fprintf stream "\n"
    done;
    close_out stream

let pixels_to_image (res_x, res_y) pixels =
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

let display (res_x, res_y) pixels =
    let open Graphics in
    open_graph (string_of_int res_x ^ "x" ^ string_of_int res_y);
    set_window_title "Ray-tracer";
    draw_image (make_image (pixels_to_image (res_x, res_y) pixels)) 0 0

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
    and rec_depth = 2 in
    let pixels = Scene.render (Scene.create (res_x, res_y) canvas_coords pos bg_color rec_depth objs lights) in
    if filename = "" then display (res_x, res_y) pixels
    else to_file filename (res_x, res_y) pixels

let main () =
    print_string "Enter source file name:\n";
    let src_filename = read_line () in
    let scene = Parser.read_file src_filename in
    let res, pixels = Scene.res scene, Scene.render scene in
    print_string "To display the scene, return. To save it to file, enter destination file name:";
    let dst_filename = read_line () in
    if dst_filename = "" then display res pixels
    else to_file dst_filename res pixels

let () = main ()
