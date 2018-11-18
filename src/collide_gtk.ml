open General.Abbr
open Collide

let _ = GMain.init ()

let window = GWindow.window ~title:"Collide" ()

let _  = window#connect#destroy ~callback:GMain.quit

let vbox = GPack.vbox ~packing:window#add ()

let buttons = GPack.hbox ~packing:(vbox#pack ~expand:false) ()

let packing = buttons#pack ~expand:false
let display_velocity_vectors_checkbox = GButton.check_button ~label:"Display velocity vectors" ~packing ()
let display_previous_positions_checkbox = GButton.check_button ~label:"Display previous positions" ~packing ()
(* @todo Put these controls in a dialog, open it with a "Randomize..." button and randomize on "OK" *)
let _ = GMisc.label ~text:"Balls: " ~packing ()
let randomize_balls = GEdit.entry ~text:(Int.to_string Application.Defaults.balls) ~width_chars:3 ~packing ()
let _ = GMisc.label ~text:"Max speed: " ~packing ()
let randomize_max_speed = GEdit.entry ~text:(Fl.to_string Application.Defaults.max_speed) ~width_chars:3 ~packing ()
let _ = GMisc.label ~text:"Radius: between: " ~packing ()
let randomize_min_radius = GEdit.entry ~text:(Fl.to_string Application.Defaults.min_radius) ~width_chars:3 ~packing ()
let _ = GMisc.label ~text:" and " ~packing ()
let randomize_max_radius = GEdit.entry ~text:(Fl.to_string Application.Defaults.max_radius) ~width_chars:3 ~packing ()
let _ = GMisc.label ~text:"Density: between: " ~packing ()
let randomize_min_density = GEdit.entry ~text:(Fl.to_string Application.Defaults.min_density) ~width_chars:3 ~packing ()
let _ = GMisc.label ~text:" and " ~packing ()
let randomize_max_density = GEdit.entry ~text:(Fl.to_string Application.Defaults.max_density) ~width_chars:3 ~packing ()
let randomize_button = GButton.button ~label:"Randomize" ~packing ()

let graphical_view = GMisc.drawing_area ~packing:vbox#add ~width:320 ~height:240 ()

let () = window#show ()

module App = GraphicalApplication.Make(struct
  module Cairo = Cairo

  let make_callback name f ret =
    fun _ ->
      begin
        try
          f ()
        with
          ex -> StdErr.print "Exception in %s: %s\n" name (Exn.to_string ex)
      end;
      ret

  module GraphicalView = struct
    let size () =
      let {Gtk.width; height; _} = graphical_view#misc#allocation in
      (width - 1, height - 1)
      (* Without the "- 1" above, we get the following error in the expose event:
        > The program 'collide_gtk.exe' received an X Window System error.
        > This probably reflects a bug in the program.
        > The error was 'BadDrawable (invalid Pixmap or Window parameter)'.
        > (Details: serial 1009 error_code 9 request_code 62 minor_code 0)
      I have no idea why but I don't want to spend time investigating right now :-/ *)

    let with_context f =
      let (w, h) = size () in
      let image = Cairo.Image.(create RGB24 ~w ~h) in
      let context = Cairo.create image in
      f ~context;
      let context = Cairo_gtk.create graphical_view#misc#window in
      let pattern = Cairo.Pattern.create_for_surface image in
      Cairo.set_source context pattern;
      Cairo.paint context

    let on_resized f =
      graphical_view#event#connect#configure ~callback:(make_callback "graphical_view.configure" (fun () -> f ~dimensions:(size ())) true)
      |> ignore
  end

  module Timer = struct
    let set_recurring ~seconds f =
      Glib.Timeout.add ~ms:(Int.of_float (1000. *. seconds)) ~callback:(make_callback "timer.timeout" f true)
      |> ignore
  end

  module Toolbar = struct
    module Display = struct
      type settings = {
        velocity_vectors: bool;
        previous_positions: int;
      }

      let settings () = {
        velocity_vectors = display_velocity_vectors_checkbox#active;
        (* @todo Allow user to specify this number *)
        previous_positions = if display_previous_positions_checkbox#active then 10 else 0;
      }
    end

    let on_display_settings_changed f = Display.(
      let callback = make_callback "foo" (fun () ->
        let {velocity_vectors; previous_positions} = Display.settings () in
        f ~velocity_vectors ~previous_positions
      ) () in
      ignore (display_velocity_vectors_checkbox#connect#toggled ~callback, display_previous_positions_checkbox#connect#toggled ~callback)
    )

    module Randomize = struct
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
          balls = randomize_balls#text |> Int.of_string;
          max_speed = randomize_max_speed#text |> Fl.of_string;
          min_radius = randomize_min_radius#text |> Fl.of_string;
          max_radius = randomize_max_radius#text |> Fl.of_string;
          min_density = randomize_min_density#text |> Fl.of_string;
          max_density = randomize_max_density#text |> Fl.of_string;
        }
    end

    let on_randomize f = Randomize.(
      randomize_button#connect#clicked ~callback:(fun _ ->
        let {balls; max_speed; min_radius; max_radius; min_density; max_density} = settings () in
        f ~balls ~max_speed ~min_radius ~max_radius ~min_density ~max_density
      )
      |> ignore
    )
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

let () = GMain.main ()
