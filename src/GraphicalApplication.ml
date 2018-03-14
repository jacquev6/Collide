open General.Abbr

module OCSR = OCamlStandard.Random

module Make(Frontend: sig
  module Cairo: JsOfOCairo.S

  module GraphicalView: sig
    val context: unit -> Cairo.context
    val size: unit -> int * int
    val on_refresh_needed: (unit -> unit) -> unit
  end

  module Timer: sig
    val set_recurring: seconds:float -> (unit -> unit) -> unit
  end

  module Toolbar: sig
    val on_save_clicked: (unit -> unit) -> unit
  end

  module File: sig
    val save: bytes -> unit
    val on_file_loaded: (bytes -> unit) -> unit
  end
end) = struct
  module Cairo = Frontend.Cairo
  module Drawer = Drawer.Make(Cairo)

  type state = {
    simulation: Simulation.t;
  }

  let state =
    let (w, h) = Frontend.GraphicalView.size () in
    let w = Fl.of_int w -. 2. *. Drawer.wall_width
    and h = Fl.of_int h -. 2. *. Drawer.wall_width in
    let simulation = Simulation.(create
      ~dimensions:(w, h)
      (
        IntRa.make 10
        |> IntRa.ToList.map ~f:(fun _ ->
          let rd a b = a +. OCSR.float (b -. a) in
          let radius = rd 3. 15. in
          Ball.{
            radius;
            density=(rd 0.1 1.);
            position=((rd radius (w -. radius)), (rd radius (h -. radius)));
            velocity=(let v_max = 100. in ((rd (-.v_max) v_max), (rd (-.v_max) v_max)));
          };
        )
      )
    ) in
    ref {simulation}

  let draw () =
    let {simulation} = !state in
    let context = Frontend.GraphicalView.context () in
    Cairo.save context;
    Drawer.draw ~context simulation;
    Cairo.restore context

  let set_simulation simulation =
    state := {simulation};
    draw ()

  let () = Frontend.GraphicalView.on_refresh_needed draw

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

  let save () =
    let {simulation} = !state in
    simulation
    |> JsonSimulation.to_json
    |> Yojson.Basic.pretty_to_string
    |> By.of_string
    |> Frontend.File.save

  let () = Frontend.Toolbar.on_save_clicked save

  let file_loaded bs =
    set_simulation (
      bs
      |> By.to_string
      |> Yojson.Basic.from_string
      |> JsonSimulation.of_json
    )

  let () = Frontend.File.on_file_loaded file_loaded
end
