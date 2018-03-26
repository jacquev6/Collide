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
        Js._false
      )
  end

  module Timer = struct
    let set_recurring ~seconds f =
      Dom_html.window##setInterval (Js.wrap_callback f) (seconds *. 1000.)
      |> ignore
  end

  module Toolbar = struct
    let _ = Js.Unsafe.eval_string {|
      jQuery("#toolbar").modal();
      jQuery("#graphical_view").on("click", function(){jQuery("#toolbar").modal()});
    |}
  end
end)
