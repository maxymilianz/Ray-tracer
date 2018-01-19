(* #use "main.ml"

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
let default_scene = 1 *)

%token <int> INT
%token <float> FLOAT
%token SPH
%token SURF
%token POINT
%token SUN
%token LEFT_BRACE
%token RIGHT_BRACE
%token EOF
%start <parser_token option> file

%%

file:
    | EOF { None }
    | c = contents { Some c }

contents:
    | r = res; cc = canvas_coords; cp = camera_pos; bgc = bg_color; rd = rec_depth; o = objs; l = lights { `Scene (r, cc, cp, bgc, rd, o, l) }

res:
    | (* empty *) { None }
    | x = INT; y = INT { Some (x, y) }

canvas_coords:
    | (* empty *) { None }
    | ul = vector; ur = vector; lr = vector; ll = vector { Some (ul, ur, lr, ll) }

camera_pos:
    | (* empty *) { None }
    | p = vector { p }

bg_color:
    | (* empty *) { None }
    | r = INT; g = INT; b = INT { Some (`Color (r, g, b)) }

rec_depth:
    | (* empty *) { None }
    | rd = INT; { Some rd }

objs:
    | (* empty *) { [] }
    | o = obj; os = objs { o :: os }

lights:
    | (* empty *) { [] }
    | l = light; ls = lights { l :: ls }

obj:
    | SPH; LEFT_BRACE; p = vector; r = FLOAT; c = color; cr = color_ratio; RIGHT_BRACE { Some (`Sph (p, r, c, cr)) }
    | SURF; LEFT_BRACE; n = vector; p = vector; c = color; cr = color_ratio; RIGHT_BRACE { Some (`Surf (n, p, c, cr)) }

light:
    | POINT; LEFT_BRACE; p = vector; i = FLOAT; RIGHT_BRACE { Some (`Point (p, i)) }
    | SUN; LEFT_BRACE; d = vector; i = FLOAT; RIGHT_BRACE { Some (`Sun (d, i)) }

vector:
    | x = FLOAT; y = FLOAT; z = FLOAT { Some (`Vector (x, y, z)) }

color:
    | r = FLOAT; g = FLOAT; b = FLOAT { Some (`Color (r, g, b)) }

color_ratio:
    | g = FLOAT; r = FLOAT; s = FLOAT { Some (g, r, s) }