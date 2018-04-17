open General.Abbr
open Collide

module OCSS = OCamlStandard.Scanf
module OCSA = OCamlStandard.Arg

let width = ref 640
and height = ref 480
and balls = ref Application.Defaults.balls
and max_speed = ref Application.Defaults.max_speed
and min_radius = ref Application.Defaults.min_radius
and max_radius = ref Application.Defaults.max_radius
and min_density = ref Application.Defaults.min_density
and max_density = ref Application.Defaults.max_density
and fps = ref 25
and duration = ref 30
and filename_format = ref None
and velocity_vectors = ref Application.Defaults.velocity_vectors
and previous_positions = ref Application.Defaults.previous_positions

let speclist =
  OCSA.[
    ("--width", Set_int width, "width");
    ("--height", Set_int height, "height");
    ("--balls", Set_int balls, "balls");
    ("--max-speed", Set_float max_speed, "max speed");
    ("--min-radius", Set_float min_radius, "min radius");
    ("--max-radius", Set_float max_radius, "max radius");
    ("--min-density", Set_float min_density, "min density");
    ("--max-density", Set_float max_density, "max density");
    ("--fps", Set_int fps, "fps");
    ("--duration", Set_int duration, "duration");
    ("--velocity-vectors", Set velocity_vectors, "display velocity vectors");
    ("--previous-positions", Set_int previous_positions, "display previous positions");
  ]

let () = OCSA.parse speclist (fun format -> Exn.failure_unless (Opt.is_none !filename_format) "filename_format must be specified exactly once"; filename_format := Some format) "Usage: collide_cli [options] filename_format\n\nfilename_format is a Printf format for a single integer (like \"%08d.png\")\n\nOptions:"

let width = !width
and height = !height
and balls = !balls
and max_speed = !max_speed
and min_radius = !min_radius
and max_radius = !max_radius
and min_density = !min_density
and max_density = !max_density
and fps = !fps
and duration = !duration
and filename_format = OCSS.format_from_string (Opt.value !filename_format ~exc:(Exn.Failure "filename_format must be specified exactly once")) "%d"
and velocity_vectors = !velocity_vectors
and previous_positions = !previous_positions

module Application = Application.Make(Cairo)

let application = Application.create
  ~dimensions:(width, height)
  ~balls ~max_speed
  ~min_radius ~max_radius
  ~min_density ~max_density
  ~velocity_vectors ~previous_positions

let frames = duration * fps

let _ = StdOut.print ~flush:true "Generating %i frames (%is at %ifps), named %s, with %i balls (radius: %g to %g, density: %g to %g, speed: 0 to %g)\n" frames duration fps (Frmt.to_string filename_format) balls min_radius max_radius min_density max_density max_speed

let _ =
  IntRa.make frames
  |> IntRa.iter ~f:(fun i ->
    let image = Cairo.Image.(create RGB24 ~width ~height) in
    let context = Cairo.create image in
    Application.draw application ~context;
    Cairo.PNG.write image (Frmt.apply filename_format i);
    let date = Fl.of_int (i + 1) /. Fl.of_int fps in
    Application.advance application ~date
  )
