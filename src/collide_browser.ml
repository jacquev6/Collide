open General.Abbr
open Collide

let get_by_id id coerce =
  Js.Opt.get (coerce (Dom_html.getElementById id)) (fun () -> Exn.(raise NotFound))


module App = GraphicalApplication.Make(struct
  module Cairo = JsOfOCairo

  module GraphicalView = struct
    let graphical_view = get_by_id "graphical_view" Dom_html.CoerceTo.canvas

    let with_context f =
      f (JsOfOCairo.create graphical_view)

    let size () =
      (graphical_view##.width, graphical_view##.height)

    let on_refresh_needed f =
      (* No refresh event on HTML canvas; just draw it now *)
      f ()

    let resize_canvas () =
      graphical_view##.height := Js.Optdef.get Dom_html.window##.innerHeight (fun () -> 320);
      graphical_view##.width := Js.Optdef.get Dom_html.window##.innerWidth (fun () -> 240)

    let () = resize_canvas ()

    let on_resized f =
      Dom_html.window##.onresize := Dom.handler (fun _ ->
        resize_canvas ();
        f ();
        Js._true
      )
  end

  module Timer = struct
    let set_recurring ~seconds f =
      Dom_html.window##setInterval (Js.wrap_callback f) (seconds *. 1000.)
      |> ignore
  end

  module Toolbar = struct
    let on_draw_velocity_set f =
      let checkbox = get_by_id "draw_velocity_checkbox" Dom_html.CoerceTo.input in
      checkbox##.onchange := Dom.handler (fun _ ->
        checkbox##.checked
        |> Js.to_bool
        |> f;
        Js._true
      )

    let on_randomize f =
      let randomize_button = get_by_id "randomize_button" Dom_html.CoerceTo.button
      and randomize_balls = get_by_id "randomize_balls" Dom_html.CoerceTo.input
      and randomize_max_speed = get_by_id "randomize_max_speed" Dom_html.CoerceTo.input
      and randomize_min_radius = get_by_id "randomize_min_radius" Dom_html.CoerceTo.input
      and randomize_max_radius = get_by_id "randomize_max_radius" Dom_html.CoerceTo.input
      and randomize_min_density = get_by_id "randomize_min_density" Dom_html.CoerceTo.input
      and randomize_max_density = get_by_id "randomize_max_density" Dom_html.CoerceTo.input in
      randomize_button##.onclick := Dom.handler (fun _ ->
        let balls = randomize_balls##.value |> Js.to_string |> Int.of_string
        and max_speed = randomize_max_speed##.value |> Js.to_string |> Fl.of_string
        and min_radius = randomize_min_radius##.value |> Js.to_string |> Fl.of_string
        and max_radius = randomize_max_radius##.value |> Js.to_string |> Fl.of_string
        and min_density = randomize_min_density##.value |> Js.to_string |> Fl.of_string
        and max_density = randomize_max_density##.value |> Js.to_string |> Fl.of_string in
        f ~balls ~max_speed ~min_radius ~max_radius ~min_density ~max_density;
        Js._true
      )

    let _ = Js.Unsafe.eval_string {|
      jQuery("#toolbar").modal();
      jQuery("#graphical_view").on("click", function(){jQuery("#toolbar").modal()});
    |}
  end
end)
