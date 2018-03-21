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
  end

  module Timer = struct
    let set_recurring ~seconds f =
      Dom_html.window##setInterval (Js.wrap_callback f) (seconds *. 1000.)
      |> ignore
  end

  module Toolbar = struct
    let on_save_clicked _ =
      ()
  end

  module File = struct
    let save _ =
      ()

    let on_file_loaded _ =
      ()
  end
end)
