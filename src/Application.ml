open General.Abbr

module Defaults = struct
  let balls = 30
  let max_speed = 100.
  let min_radius = 3.
  let max_radius = 15.
  let min_density = 0.1
  let max_density = 1.
  let velocity_vectors = false
  let previous_positions = 0
end

module Make(C: JsOfOCairo.S) = struct
  module Drawer = Drawer.Make(C)

  type t = {
    mutable simulations: Simulation.t Ring.t;
    mutable velocity_vectors: bool;
  }

  let previous_positions_step = 10 (* @todo Make this depend on frame rate. Currently OK for 25fps. *)

  let make_ring_size ~previous_positions =
    Exn.failure_unless (previous_positions >= 0) "previous positions must be greater than or equal to 0 (got %i)" previous_positions;
    1 + previous_positions_step * previous_positions

  let make_dimensions ~dimensions:(width, height) =
    Exn.failure_unless (0 < width) "width must be greater than 0 (got %i)" width;
    Exn.failure_unless (0 < height) "height must be greater than 0 (got %i)" height;
    (Fl.of_int width, Fl.of_int height)

  let make_random_simulation ~dimensions ~balls ~max_speed ~min_radius ~max_radius ~min_density ~max_density =
    Exn.failure_unless (0 < balls) "balls must be greater than 0 (got %i)" balls;
    Exn.failure_unless (0. < min_radius) "min radius must be greater than 0. (got %g)" min_radius;
    Exn.failure_unless (min_radius <= max_radius) "max radius must be greater or equal to --min_radius. (got %g and %g)" max_radius min_radius;
    Exn.failure_unless (0. < min_density) "min density must be greater than 0. (got %g)" min_density;
    Exn.failure_unless (min_density <= max_density) "max density must be greater or equal to --min_density. (got %g and %g)" max_density min_density;
    Exn.failure_unless (max_density <= 1.) "max density must be less than or equal to 1. (got %g)" max_density;
    Simulation.randomize ~dimensions ~balls ~max_speed ~min_radius ~max_radius ~min_density ~max_density

  let create
    ~dimensions
    ~balls ~max_speed
    ~min_radius ~max_radius
    ~min_density ~max_density
    ~velocity_vectors ~previous_positions
  =
    let dimensions = make_dimensions ~dimensions in
    let simulation = make_random_simulation ~dimensions ~balls ~max_speed ~min_radius ~max_radius ~min_density ~max_density in
    let simulations = Ring.(make_ring_size ~previous_positions |> make |> add ~v:simulation) in
    {simulations; velocity_vectors}

  let date {simulations; _} =
    let simulation = Ring.get simulations ~index:0 in
    Simulation.date simulation

  let resize app ~dimensions =
    let dimensions = make_dimensions ~dimensions in
    let simulation = Ring.get app.simulations ~index:0 in
    let simulation = Simulation.resize simulation ~dimensions in
    app.simulations <- Ring.(max_size app.simulations |> make |> add ~v:simulation)

  let set_display app ~velocity_vectors ~previous_positions =
    app.velocity_vectors <- velocity_vectors;
    let new_ring_size = make_ring_size ~previous_positions in
    if new_ring_size <> Ring.max_size app.simulations then begin
      let simulation = Ring.get app.simulations ~index:0 in
      app.simulations <- Ring.(new_ring_size |> make |> add ~v:simulation)
    end

  let randomize app ~balls ~max_speed ~min_radius ~max_radius ~min_density ~max_density =
    let simulation = Ring.get app.simulations ~index:0 in
    let dimensions = Simulation.dimensions simulation in
    let simulation = make_random_simulation ~dimensions ~balls ~max_speed ~min_radius ~max_radius ~min_density ~max_density in
    app.simulations <- Ring.(max_size app.simulations |> make |> add ~v:simulation)

  let advance app ~date =
    let simulation = Ring.get app.simulations ~index:0 in
    let simulation = Tu2.get_1 @@ Simulation.advance simulation ~date in
    app.simulations <- Ring.add app.simulations ~v:simulation

  let draw {simulations; velocity_vectors} ~context =
    C.save context;
    C.set_source_rgb context ~r:1. ~g:1. ~b:1.;
    C.paint context;
    Drawer.draw ~context ~velocity_vectors (Ring.get simulations ~index:0);
    let size = Ring.size simulations in
    IntRa.make ~start:previous_positions_step size ~step:previous_positions_step
    |> IntRa.iter ~f:(fun index ->
      let alpha = 1. -. 0.9 *. Fl.of_int index /. Fl.of_int size in
      Drawer.draw ~context ~velocity_vectors:false ~alpha (Ring.get simulations ~index)
    );
    C.restore context
end
