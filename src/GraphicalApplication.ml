open General.Abbr

module Make(Frontend: sig
  module Cairo: JsOfOCairo.S

  val initialize_app: (dimensions:int * int
    -> balls:int -> max_speed:float
    -> min_radius:float -> max_radius:float
    -> min_density:float -> max_density:float
    -> velocity_vectors:bool -> previous_positions:int
    -> 'a) -> 'a

  module GraphicalView: sig
    val with_context: (context:Cairo.context -> unit) -> unit
    val on_resized: (dimensions:(int * int) -> unit) -> unit
  end

  module Timer: sig
    val set_recurring: seconds:float -> (unit -> unit) -> unit
  end

  module Toolbar: sig
    val on_display_settings_changed: (velocity_vectors:bool -> previous_positions:int -> unit) -> unit
    val on_randomize: (balls:int -> max_speed:float -> min_radius:float -> max_radius:float -> min_density:float -> max_density:float -> unit) -> unit
  end
end) = struct
  module Cairo = Frontend.Cairo
  module Application = Application.Make(Cairo)

  let application = Frontend.initialize_app Application.create

  let resize ~dimensions =
    Application.resize application ~dimensions

  let () = Frontend.GraphicalView.on_resized resize

  let interval = 1. /. 25.

  let advance () =
    let date = Application.date application +. interval in
    Application.advance application ~date;
    Frontend.GraphicalView.with_context (Application.draw application)

  let () = Frontend.Timer.set_recurring ~seconds:interval advance

  let set_display = Application.set_display application

  let () = Frontend.Toolbar.on_display_settings_changed set_display

  let randomize = Application.randomize application

  let () = Frontend.Toolbar.on_randomize randomize
end
