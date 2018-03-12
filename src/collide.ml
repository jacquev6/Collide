open General.Abbr

module Drawer = Drawer.Make(Cairo)

let simulation = Simulation.(create
  ~dimensions:(640., 480.)
  Ball.[
    {radius=10.; density=1.; position=(320., 240.); speed=(1., 1.3)};
    {radius=15.; density=0.; position=(120., 240.); speed=(2.3, 1.3)};
  ]
)

let draw_frame format =
  Frmt.with_result format ~f:(fun file_name simulation ->
    let (w, h) = Simulation.dimensions simulation in
    let image = Cairo.Image.(create RGB24 ~width:(Int.of_float w) ~height:(Int.of_float h)) in
    let context = Cairo.create image in
    Cairo.set_source_rgb context ~r:1. ~g:1. ~b:1.;
    Cairo.paint context;
    Drawer.draw ~context simulation;
    Cairo.PNG.write image file_name
  )

let () = draw_frame "frame.png" simulation
