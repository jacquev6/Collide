open General.Abbr
open Collide

let () = Exn.record_backtraces true

module Drawer = Drawer.Make(Cairo)

let settings = {
  Drawer.Settings.draw_velocity = false;
}

let width = 640
and height = 480

let simulation = Simulation.randomize
  ~dimensions:(Fl.of_int width, Fl.of_int height)
  ~balls:30 ~max_speed:100.
  ~min_radius:3. ~max_radius:15.
  ~min_density:0.1 ~max_density:1.

let images_per_second = Int.of_string OCamlStandard.Sys.argv.(1)
and duration = Int.of_string OCamlStandard.Sys.argv.(2)

let frames = duration * images_per_second + 1

let () = assert (frames <= 99999999) (* Because we need zero-padded filenames for ffmpeg and use %08d below. *)

let _ =
  IntRa.make frames
  |> IntRa.fold ~init:simulation ~f:(fun simulation i ->
    let image = Cairo.Image.(create RGB24 ~width ~height) in
    let context = Cairo.create image in
    Drawer.draw ~context ~settings simulation;
    Cairo.PNG.write image (Frmt.apply "%08d.png" i);
    let date = Fl.of_int i /. Fl.of_int images_per_second in
    Simulation.advance simulation ~date
    |> Tu2.get_1
  )
