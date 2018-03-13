open General.Abbr
open Collide_

let () = Exn.record_backtraces true

module Drawer = Drawer.Make(Cairo)

let draw_frame ?event format =
  Frmt.with_result format ~f:(fun file_name simulation ->
    let (w, h) = Drawer.measure simulation in
    let image = Cairo.Image.(create RGB24 ~width:(Int.of_float w) ~height:(Int.of_float h)) in
    let context = Cairo.create image in
    Cairo.set_source_rgb context ~r:0.9 ~g:0.9 ~b:1.;
    Cairo.paint context;
    Drawer.draw ~context ?event simulation;
    Cairo.PNG.write image file_name
  )

let () = begin
  let simulation = Simulation.(create
    ~dimensions:(640., 480.)
    Ball.[
      {radius=40.; density=1.; position=(320., 240.); velocity=(100., 90.)};
    ]
  ) in
  let images_per_second = Int.of_string OCamlStandard.Sys.argv.(1)
  and duration = Int.of_string OCamlStandard.Sys.argv.(2) in
  IntRa.make (duration * images_per_second + 1)
  |> IntRa.fold ~init:simulation ~f:(fun simulation i ->
    let max_date = Fl.of_int i /. Fl.of_int images_per_second in
    let rec aux j simulation =
      let (event, simulation) = Simulation.advance simulation ~max_date in
      match event with
        | None -> draw_frame "%08d.png" i simulation; simulation
        | Some event -> draw_frame ~event "%08d.event_%04d.png" i j simulation; aux (j + 1) simulation
    in
    aux 0 simulation
  )
  |> ignore
end
