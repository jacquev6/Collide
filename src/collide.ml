open General.Abbr

module Drawer = Drawer.Make(Cairo)

let draw_frame format =
  Frmt.with_result format ~f:(fun file_name simulation ->
    let (w, h) = Simulation.dimensions simulation in
    let image = Cairo.Image.(create RGB24 ~width:(Int.of_float w) ~height:(Int.of_float h)) in
    let context = Cairo.create image in
    Cairo.set_source_rgb context ~r:0.9 ~g:0.9 ~b:1.;
    Cairo.paint context;
    Drawer.draw ~context simulation;
    Cairo.PNG.write image file_name
  )

let () = begin
  let simulation = ref Simulation.(create
    ~date:0.
    ~dimensions:(640., 480.)
    Ball.[
      {radius=40.; density=1.; position=(320., 240.); speed=(10., 15.)};
    ]
  ) in
  for i = 0 to 20 do
    let max_date = Fl.of_int i in
    let (event, sim) = Simulation.advance !simulation ~max_date in
    simulation := sim;
    match event with
      | None ->
        draw_frame "%09.2f_normal.png" (Simulation.date sim) sim
      | Some _ ->
        draw_frame "%09.2f_event.png" (Simulation.date sim) sim
  done
end
