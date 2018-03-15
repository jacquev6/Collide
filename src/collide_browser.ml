open General.Abbr
open Collide

let get_by_id id coerce =
  Js.Opt.get (coerce (Dom_html.getElementById id)) (fun () -> Exn.(raise NotFound))

module App = GraphicalApplication.Make(struct
  module Cairo = JsOfOCairo

  module GraphicalView = struct
    let graphical_view = get_by_id "graphical_view" Dom_html.CoerceTo.canvas

    let context () =
      JsOfOCairo.create graphical_view

    let size () =
      (640, 480)

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
    let on_save_clicked f =
      let button = get_by_id "save_button" Dom_html.CoerceTo.button in
      button##.onclick := Dom.handler (fun _ -> f (); Js._false)
  end

  module File = struct
    let (file: (Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t -> File.file Js.t) Js.constr) =
      Js.Unsafe.global##._File

    let save bs =
      let file = new%js file (Js.array [|bs |> By.to_string |> Js.string|]) (Js.string "balls.collide") in
      Js.Unsafe.global##.FileSaver##saveAs file

    let on_file_loaded f =
      let input = get_by_id "load_input" Dom_html.CoerceTo.input in
      input##.onchange := Dom.handler (fun _ ->
        let reader = new%js File.fileReader in
        reader##.onload := Dom.handler (fun event ->
          Js.Opt.iter event##.target (fun target ->
            Js.Opt.iter (File.CoerceTo.string target##.result) (fun result ->
              result
              |> Js.to_string
              |> By.of_string
              |> f
            )
          );
          Js._false
        );
        Js.Optdef.iter (get_by_id "load_input" Dom_html.CoerceTo.input)##.files (fun files ->
          Js.Opt.iter (files##item 0) (fun file ->
            reader##readAsBinaryString file
          )
        );
        Js._false
      )
  end
end)
