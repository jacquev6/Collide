open General.Abbr
open Collide

let () = Exn.record_backtraces true

module Drawer = Drawer.Make(Cairo)

let draw_frame ?event format =
  Frmt.with_result format ~f:(fun file_name simulation ->
    let (w, h) = Simulation.dimensions simulation in
    let image = Cairo.Image.(create RGB24 ~width:(Int.of_float w) ~height:(Int.of_float h)) in
    let context = Cairo.create image in
    Drawer.draw ~context ?event simulation;
    Cairo.PNG.write image file_name
  )

let () = begin
  let simulation = Simulation.randomize ~dimensions:(640., 480.) ~balls:30 ~max_speed:100. ~min_radius:3. ~max_radius:15. ~min_density:0.1 ~max_density:1. in
  let images_per_second = Int.of_string OCamlStandard.Sys.argv.(1)
  and duration = Int.of_string OCamlStandard.Sys.argv.(2) in
  let frames = duration * images_per_second + 1 in
  assert (frames <= 99999999); (* Because we need zero-padded filenames for ffmpeg and use %08d below. *)
  IntRa.make frames
  |> IntRa.fold ~init:simulation ~f:(fun simulation i ->
    let max_date = Fl.of_int i /. Fl.of_int images_per_second in
    let rec aux simulation =
      let (event, simulation) = Simulation.advance simulation ~max_date in
      match event with
        | None -> draw_frame "%08d.png" i simulation; simulation
        | Some event -> draw_frame ~event "%06.2f.png" (Simulation.date simulation) simulation; aux simulation
    in
    aux simulation
  )
  |> ignore
end
