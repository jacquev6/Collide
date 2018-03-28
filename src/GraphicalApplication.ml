open General.Abbr

module Make(Frontend: sig
  module Cairo: JsOfOCairo.S

  module GraphicalView: sig
    val with_context: (Cairo.context -> unit) -> unit
    val size: unit -> int * int
    val on_refresh_needed: (unit -> unit) -> unit
    val on_resized: (unit -> unit) -> unit
  end

  module Timer: sig
    val set_recurring: seconds:float -> (unit -> unit) -> unit
  end

  module Toolbar: sig
    val on_display_velocity_vectors_set: (bool -> unit) -> unit
    val on_display_previous_positions_set: (bool -> unit) -> unit
    val on_randomize: (balls:int -> max_speed:float -> min_radius:float -> max_radius:float -> min_density:float -> max_density:float -> unit) -> unit
  end
end) = struct
  module Cairo = Frontend.Cairo
  module Drawer = Drawer.Make(Cairo)

  type state = {
    simulations: Simulation.t Ring.t;
    display_velocity_vectors: bool;
  }

  let get_dimensions () =
    let (w, h) = Frontend.GraphicalView.size () in
    (Fl.of_int w, Fl.of_int h)

  let state =
    let dimensions = get_dimensions () in
    (* @todo Deduplicate these numbers: they appear in collide_gtk.ml, collide_browser.html and here *)
    let simulation = Simulation.randomize ~dimensions ~balls:10 ~max_speed:100. ~min_radius:3. ~max_radius:10. ~min_density:0.1 ~max_density:1. in
    let simulations = Ring.(make 1 |> add ~v:simulation) in
    ref {simulations; display_velocity_vectors=false}

  let draw () =
    Frontend.GraphicalView.with_context (fun context ->
      let {simulations; display_velocity_vectors} = !state in
      Cairo.save context;
      Cairo.set_source_rgb context ~r:1. ~g:1. ~b:1.;
      Cairo.paint context;
      Drawer.draw ~context ~display_velocity_vectors (Ring.get simulations ~index:0);
      let step = 10 and size = Ring.size simulations in
      IntRa.make ~start:step size ~step
      |> IntRa.iter ~f:(fun index ->
        let alpha = 1. -. Fl.of_int index /. Fl.of_int size in
        Drawer.draw ~context ~display_velocity_vectors:false ~alpha (Ring.get simulations ~index)
      );
      Cairo.restore context
    )

  let () = Frontend.GraphicalView.on_refresh_needed draw

  let get_simulation () =
    let {simulations; _} = !state in
    Ring.get simulations ~index:0

  let add_simulation simulation =
    let {simulations; _} = !state in
    let simulations = Ring.add simulations ~v:simulation in
    state := {!state with simulations}

  let reset_simulation simulation =
    let {simulations; _} = !state in
    let simulations = Ring.(make (max_size simulations) |> add ~v:simulation) in
    state := {!state with simulations}

  let resize () =
    let simulation = get_simulation ()
    and dimensions = get_dimensions () in
    reset_simulation (Simulation.resize ~dimensions simulation)

  let () = Frontend.GraphicalView.on_resized resize

  let interval = 1. /. 25.

  let advance () =
    let simulation = get_simulation () in
    let date = Simulation.date simulation +. interval in
    add_simulation (Simulation.advance ~date simulation |> Tu2.get_1);
    draw ()

  let () = Frontend.Timer.set_recurring ~seconds:interval advance

  let set_display_velocity_vectors display_velocity_vectors =
    state := {!state with display_velocity_vectors}

  let () = Frontend.Toolbar.on_display_velocity_vectors_set set_display_velocity_vectors

  let set_display_previous_positions display_previous_positions =
    let simulation = get_simulation () in
    let s = if display_previous_positions then 100 else 1 in
    let simulations = Ring.(make s |> add ~v:simulation) in
    state := {!state with simulations}

  let () = Frontend.Toolbar.on_display_previous_positions_set set_display_previous_positions

  let randomize ~balls ~max_speed ~min_radius ~max_radius ~min_density ~max_density =
    let dimensions = get_dimensions () in
    reset_simulation (Simulation.randomize ~dimensions ~balls ~max_speed ~min_radius ~max_radius ~min_density ~max_density)

  let () = Frontend.Toolbar.on_randomize randomize
end
