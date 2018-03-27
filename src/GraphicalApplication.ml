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
    val on_draw_velocity_set: (bool -> unit) -> unit
  end
end) = struct
  module Cairo = Frontend.Cairo
  module Drawer = Drawer.Make(Cairo)

  type state = {
    simulation: Simulation.t;
    settings: Drawer.Settings.t;
  }

  let get_dimensions () =
    let (w, h) = Frontend.GraphicalView.size () in
    (Fl.of_int w, Fl.of_int h)

  let state =
    let dimensions = get_dimensions () in
    let simulation = Simulation.randomize ~dimensions ~balls:10 ~max_speed:100. ~min_radius:3. ~max_radius:10. ~min_density:0.1 ~max_density:1.
    and settings = {Drawer.Settings.draw_velocity=false} in
    ref {simulation; settings}

  let draw () =
    Frontend.GraphicalView.with_context (fun context ->
      let {simulation; settings} = !state in
      Cairo.save context;
      Drawer.draw ~context ~settings simulation;
      Cairo.restore context
    )

  let set_simulation simulation =
    state := {!state with simulation};
    draw ()

  let set_settings settings =
    state := {!state with settings};
    draw ()

  let () = Frontend.GraphicalView.on_refresh_needed draw

  let resize () =
    let {simulation; _} = !state in
    let dimensions = get_dimensions () in
    set_simulation (Simulation.resize ~dimensions simulation)

  let () = Frontend.GraphicalView.on_resized resize

  let interval = 1. /. 25.

  let advance () =
    let {simulation; _} = !state in
    let date = Simulation.date simulation +. interval in
    set_simulation (Simulation.advance ~date simulation |> Tu2.get_1)

  let () = Frontend.Timer.set_recurring ~seconds:interval advance

  let set_draw_velocity draw_velocity =
    set_settings {Drawer.Settings.draw_velocity}

  let () = Frontend.Toolbar.on_draw_velocity_set set_draw_velocity
end
