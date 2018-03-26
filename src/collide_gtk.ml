open General.Abbr
open Collide

let _ = GMain.init ()

let window = GWindow.window ~title:"Collide" ()

let _  = window#connect#destroy ~callback:GMain.quit

let vbox = GPack.vbox ~packing:window#add ()

(* let buttons = GPack.hbox ~packing:(vbox#pack ~expand:false) () *)

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
      let (width, height) = size () in
      let image = Cairo.Image.(create RGB24 ~width ~height) in
      let context = Cairo.create image in
      f context;
      let context = Cairo_gtk.create graphical_view#misc#window in
      let pattern = Cairo.Pattern.create_for_surface image in
      Cairo.set_source context pattern;
      Cairo.paint context

    let on_refresh_needed f =
      graphical_view#event#connect#expose ~callback:(make_callback "graphical_view.expose" f true)
      |> ignore

    let on_resized f =
      graphical_view#event#connect#configure ~callback:(make_callback "graphical_view.configure" f true)
      |> ignore
  end

  module Timer = struct
    let set_recurring ~seconds f =
      Glib.Timeout.add ~ms:(Int.of_float (1000. *. seconds)) ~callback:(make_callback "timer.timeout" f true)
      |> ignore
  end
end)

let () = GMain.main ()
