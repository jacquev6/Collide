open General.Abbr

module OCSR = OCamlStandard.Random

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

  let (get_dimensions: unit -> float * float) = fun () ->
    let (w, h) = Frontend.GraphicalView.size () in
    (Fl.of_int w -. 2. *. Drawer.wall_width, Fl.of_int h -. 2. *. Drawer.wall_width)

  let state =
    let dimensions = get_dimensions () in
    let simulation = Simulation.create
      ~dimensions
      (
        IntRa.make 10
        |> IntRa.ToList.map ~f:(fun _ ->
          let rd a b = a +. OCSR.float (b -. a) in
          let radius = rd 3. 15. in
          Simulation.Ball.{
            radius;
            density=(rd 0.1 1.);
            position=(let (w, h) = dimensions in ((rd radius (w -. radius)), (rd radius (h -. radius))));
            velocity=(let v_max = 100. in ((rd (-.v_max) v_max), (rd (-.v_max) v_max)));
          };
        )
      )
    in
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
