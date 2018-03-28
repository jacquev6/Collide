open General.Abbr
open Collide

module OCSS = OCamlStandard.Scanf
module OCSA = OCamlStandard.Arg

let width = ref 640
and height = ref 480
and balls = ref 30
and max_speed = ref 100.
and min_radius = ref 3.
and max_radius = ref 15.
and min_density = ref 0.1
and max_density = ref 1.
and fps = ref 25
and duration = ref 30
and filename_format = ref None
and display_velocity_vectors = ref false

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
    ("--display-velocity-vectors", Set display_velocity_vectors, "display velocity vectors");
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
and display_velocity_vectors = !display_velocity_vectors

let () = begin
  Exn.failure_unless (0 < width) "--width must be greater than 0 (got %i)" width;
  Exn.failure_unless (0 < height) "--height must be greater than 0 (got %i)" height;
  Exn.failure_unless (0 < balls) "--balls must be greater than 0 (got %i)" balls;
  Exn.failure_unless (0. < min_radius) "--min-radius must be greater than 0. (got %g)" min_radius;
  Exn.failure_unless (min_radius <= max_radius) "--max-radius must be greater or equal to --min_radius. (got %g and %g)" max_radius min_radius;
  Exn.failure_unless (0. < min_density) "--min-density must be greater than 0. (got %g)" min_density;
  Exn.failure_unless (min_density <= max_density) "--max-density must be greater or equal to --min_density. (got %g and %g)" max_density min_density;
  Exn.failure_unless (max_density <= 1.) "--max-density must be less than or equal to 1. (got %g)" max_density;
end

let frames = duration * fps

let _ = StdOut.print ~flush:true "Generating %i frames (%is at %ifps), named %s, with %i balls (radius: %g to %g, density: %g to %g, speed: 0 to %g)\n" frames duration fps (Frmt.to_string filename_format) balls min_radius max_radius min_density max_density max_speed

let simulation = Simulation.randomize
  ~dimensions:(Fl.of_int width, Fl.of_int height)
  ~balls ~max_speed
  ~min_radius ~max_radius
  ~min_density ~max_density

module Drawer = Drawer.Make(Cairo)

let _ =
  IntRa.make frames
  |> IntRa.fold ~init:simulation ~f:(fun simulation i ->
    let image = Cairo.Image.(create RGB24 ~width ~height) in
    let context = Cairo.create image in
    Cairo.set_source_rgb context ~r:1. ~g:1. ~b:1.;
    Cairo.paint context;
    Drawer.draw ~context ~display_velocity_vectors simulation;
    Cairo.PNG.write image (Frmt.apply filename_format i);
    let date = Fl.of_int i /. Fl.of_int fps in
    Simulation.advance simulation ~date
    |> Tu2.get_1
  )
