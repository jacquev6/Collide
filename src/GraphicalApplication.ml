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
end) = struct
  module Cairo = Frontend.Cairo
  module Drawer = Drawer.Make(Cairo)

  type state = {
    simulation: Simulation.t;
  }

  let get_dimensions () =
    let (w, h) = Frontend.GraphicalView.size () in
    (Fl.of_int w, Fl.of_int h)

  let state =
    let dimensions = get_dimensions () in
    let simulation = Simulation.randomize ~dimensions ~balls:10 ~max_speed:100. ~min_radius:3. ~max_radius:10. ~min_density:0.1 ~max_density:1. in
    ref {simulation}

  let draw () =
    Frontend.GraphicalView.with_context (fun context ->
      let {simulation} = !state in
      Cairo.save context;
      Drawer.draw ~context simulation;
      Cairo.restore context
    )

  let set_simulation simulation =
    state := {simulation};
    draw ()

  let () = Frontend.GraphicalView.on_refresh_needed draw

  let resize () =
    let {simulation} = !state in
    let dimensions = get_dimensions () in
    set_simulation (Simulation.resize ~dimensions simulation)

  let () = Frontend.GraphicalView.on_resized resize

  let interval = 1. /. 25.

  let advance () =
    let {simulation} = !state in
    let max_date = Simulation.date simulation +. interval in
    let rec aux simulation =
      let (event, simulation) = Simulation.advance simulation ~max_date in
      match event with
        | None -> simulation
        | Some _ -> aux simulation
    in
    set_simulation (aux simulation)

  let () = Frontend.Timer.set_recurring ~seconds:interval advance
end
