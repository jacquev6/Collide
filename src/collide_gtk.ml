open General.Abbr
open Collide

let _ = GMain.init ()

let window = GWindow.window ~title:"Collide" ()

let _  = window#connect#destroy ~callback:GMain.quit

let vbox = GPack.vbox ~packing:window#add ()

let buttons = GPack.hbox ~packing:(vbox#pack ~expand:false) ()

let save_button = GButton.button ~label:"Save" ~packing:(buttons#pack ~expand:false) ()
let load_button = GButton.button ~label:"Load" ~packing:(buttons#pack ~expand:false) ()

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

  module Toolbar = struct
    let on_save_clicked f =
      save_button#connect#clicked ~callback:(make_callback "save_button.clicked" f ())
      |> ignore
  end

  module File = struct
    let filter =
      GFile.filter ~name:"Collide files" ~patterns:["*.collide"] ()

    (* @todo Factorize choice of filename *)
    let save s =
      let dialog = GWindow.file_chooser_dialog ~action:`SAVE ~title:"Save" ~parent:window () in
      dialog#add_button_stock `CANCEL `CANCEL;
      dialog#add_select_button_stock `SAVE `SAVE;
      dialog#add_filter filter;
      begin match dialog#run () with
        | `SAVE -> begin
          dialog#filename
          |> Opt.iter ~f:(fun filename ->
            OutFile.with_channel filename ~f:(fun chan ->
              OutCh.output chan s
            )
          )
        end
        | `DELETE_EVENT | `CANCEL ->
          ()
      end;
      dialog#destroy ()

    let on_file_loaded f =
      load_button#connect#clicked ~callback:(fun _ ->
        let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~title:"Open" ~parent:window () in
        dialog#add_button_stock `CANCEL `CANCEL;
        dialog#add_select_button_stock `OPEN `OPEN;
        dialog#add_filter filter;
        let filename =
          match dialog#run () with
            | `OPEN -> begin
              dialog#filename
            end
            | `DELETE_EVENT | `CANCEL ->
              None
        in
        dialog#destroy ();
        filename
        |> Opt.iter ~f:(fun filename ->
          InFile.with_channel filename ~f:(fun chan ->
            let len = 1024 * 1024 in
            let bs = By.make ~len in
            let l = OCamlStandard.Pervasives.input chan bs 0 len in
            assert (l < len); (* @todo Handle larger files *)
            let bs = OCamlStandard.Bytes.extend bs 0 (l - len) in
            f bs
          )
        )
      )
      |> ignore
  end
end)

let () = GMain.main ()
