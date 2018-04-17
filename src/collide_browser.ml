open General.Abbr
open Collide

let get_by_id id coerce =
  Js.Opt.get (coerce (Dom_html.getElementById id)) (fun () -> Exn.(raise NotFound))


module App = GraphicalApplication.Make(struct
  module Cairo = JsOfOCairo

  module GraphicalView = struct
    let canvas = get_by_id "graphical_view" Dom_html.CoerceTo.canvas

    let with_context f =
      let context = JsOfOCairo.create canvas in
      f ~context

    let size () =
      (canvas##.width, canvas##.height)

    let resize_canvas () =
      canvas##.height := Js.Optdef.get Dom_html.window##.innerHeight (fun () -> 320);
      canvas##.width := Js.Optdef.get Dom_html.window##.innerWidth (fun () -> 240)

    let () = resize_canvas ()

    let on_resized f =
      Dom_html.window##.onresize := Dom.handler (fun _ ->
        resize_canvas ();
        f ~dimensions:(size ());
        Js._true
      )
  end

  module Timer = struct
    let set_recurring ~seconds f =
      Dom_html.window##setInterval (Js.wrap_callback f) (seconds *. 1000.)
      |> ignore
  end

  module Toolbar = struct
    module Display = struct
      let velocity_vectors = get_by_id "display_velocity_vectors" Dom_html.CoerceTo.input
      let previous_positions = get_by_id "display_previous_positions" Dom_html.CoerceTo.input

      type settings = {
        velocity_vectors: bool;
        previous_positions: int;
      }

      let settings () = {
        velocity_vectors = velocity_vectors##.checked |> Js.to_bool;
        (* @todo Allow user to specify this number *)
        previous_positions = if (previous_positions##.checked |> Js.to_bool) then 10 else 0;
      }
    end

    let on_display_settings_changed f = Display.(
      let handler = Dom.handler (fun _ ->
        let {velocity_vectors; previous_positions} = Display.settings () in
        f ~velocity_vectors ~previous_positions;
        Js._true
      ) in
      velocity_vectors##.onchange := handler;
      previous_positions##.onchange := handler
    )

    module Randomize = struct
      let button = get_by_id "randomize_button" Dom_html.CoerceTo.button
      (* @todo Remove values from html file, and initialize them using Application.Defaults *)
      let balls = get_by_id "randomize_balls" Dom_html.CoerceTo.input
      let max_speed = get_by_id "randomize_max_speed" Dom_html.CoerceTo.input
      let min_radius = get_by_id "randomize_min_radius" Dom_html.CoerceTo.input
      let max_radius = get_by_id "randomize_max_radius" Dom_html.CoerceTo.input
      let min_density = get_by_id "randomize_min_density" Dom_html.CoerceTo.input
      let max_density = get_by_id "randomize_max_density" Dom_html.CoerceTo.input

      type settings = {
        balls: int;
        max_speed: float;
        min_radius: float;
        max_radius: float;
        min_density: float;
        max_density: float;
      }

      let settings () =
        {
          balls = balls##.value |> Js.to_string |> Int.of_string;
          max_speed = max_speed##.value |> Js.to_string |> Fl.of_string;
          min_radius = min_radius##.value |> Js.to_string |> Fl.of_string;
          max_radius = max_radius##.value |> Js.to_string |> Fl.of_string;
          min_density = min_density##.value |> Js.to_string |> Fl.of_string;
          max_density = max_density##.value |> Js.to_string |> Fl.of_string;
        }
    end

    let on_randomize f = Randomize.(
      button##.onclick := Dom.handler (fun _ ->
        let {balls; max_speed; min_radius; max_radius; min_density; max_density} = settings () in
        f ~balls ~max_speed ~min_radius ~max_radius ~min_density ~max_density;
        Js._true
      )
    )

    let _ = Js.Unsafe.eval_string {|
      jQuery("#toolbar").modal("show");
      jQuery("#graphical_view").on("click", function() {
        jQuery("#toolbar").modal("show");
      });
      jQuery("#show_randomize_dialog_button").on("click", function() {
        jQuery("#toolbar").modal("hide");
        jQuery("#randomize_dialog").modal("show");
      })
    |}
  end

  let initialize_app create =
    let dimensions = GraphicalView.size ()
    and {Toolbar.Randomize.balls; max_speed; min_radius; max_radius; min_density; max_density} = Toolbar.Randomize.settings ()
    and {Toolbar.Display.velocity_vectors; previous_positions} = Toolbar.Display.settings () in
    create
      ~dimensions
      ~balls ~max_speed
      ~min_radius ~max_radius
      ~min_density ~max_density
      ~velocity_vectors ~previous_positions
end)
