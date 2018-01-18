#use "main.ml"

(* scene-related *)
let default_res = 1280, 720
let default_canvas_coords = Vector.create 0. 720. 0., Vector.create 1280. 720. 0., Vector.create 1280. 0. 0., Vector.create 0. 0. 0.
let default_camera_pos = Vector.create 640. 360. (-500.)
let default_bg_color = Color.black
let default_rec_depth = 4
let default_objs = []
let default_lights = []

(* obj-related *)
let default_color = Color.red
let default_color_ratio = 0., 0.6, 0.4

(* sphere-related *)
let default_pos = Vector.create 640. 360. 500.
let default_radius = 300.

(* surface- and light- related *)
let default_normal = Vector.create 0. (-1.) 0.
let default_point = Vector.create 0. 720. 0.

(* light-related *)
let default_intensity = 1.

(* scene *)
let default_scene = 1

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token SPH
%token SURF
%token POINT
%token SUN
%token LEFT_BRACE
%token RIGTH_BRACE
%token EOF
%start file

%%

file:
    | EOF { default_scene }
    | c = contents { c }
    ;

contents:
    | r = res; cc = canvas_coords; cp = camera_pos; bgc = bg_color; rd = rec_depth; o = objs; l = lights { Scene.create r cc cp bgc rd o l }

res:
    | (* empty *) { default_res }
    | x = INT; y = INT { (x, y) }

canvas_coords:
    | (* empty *) { default_canvas_coords }
    | ul = vector; ur = vector; lr = vector; ll = vector { (ul, ur, lr, ll) }

camera_pos:
    | (* empty *) { default_camera_pos }
    | p = vector { p }

bg_color:
    | (* empty *) { default_bg_color }
    | r = INT; g = INT; b = INT { Color.create r g b }

rec_depth:
    | (* empty *) { default_rec_depth }
    | rd = INT; { rd }

objs:
    | (* empty *) { default_objs }
    | o = obj; os = objs { o :: os }

lights:
    | (* empty *) { default_lights }
    | l = light; ls = lights { l :: ls }

obj:
    | SPH; LEFT_BRACE; p = vector; r = FLOAT; c = color; cr = color_ratio; RIGTH_BRACE { Obj.create_sph p r c cr }
    | SURF; LEFT_BRACE; n = vector; p = vector; c = color; cr = color_ratio; RIGTH_BRACE { Obj.create_surf n p c cr }

light:
    | POINT; LEFT_BRACE; p = vector; i = FLOAT; RIGTH_BRACE { Light.create_point p i }
    | SUN; LEFT_BRACE; d = vector; i = FLOAT; RIGTH_BRACE { Light.create_sun d i }

vector:
    | x = FLOAT; y = FLOAT; z = FLOAT { Vector.create x y z }

color:
    | r = FLOAT; g = FLOAT; b = FLOAT { Color.create r g b }

color_ratio:
    | g = FLOAT; r = FLOAT; s = FLOAT { (g, r, s) }