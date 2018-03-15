open General.Abbr
open Collide

let () = StdOut.print "This will soon be a GTK+ application\n"

let _ = GMain.init ()

let window = GWindow.window ~title:"SimOptics" ~width:640 ~height:480 ()

let _  = window#connect#destroy ~callback:GMain.quit

let vbox = GPack.vbox ~packing:window#add ()

let buttons = GPack.hbox ~packing:(vbox#pack ~expand:false) ()

let save_button = GButton.button ~label:"Save" ~packing:(buttons#pack ~expand:false) ()
let load_button = GButton.button ~label:"Load" ~packing:(buttons#pack ~expand:false) ()

let graphical_view = GMisc.drawing_area ~packing:vbox#add ()

let () = window#show ()

module App = GraphicalApplication.Make(struct
  module Cairo = Cairo

  module GraphicalView = struct
    let context () =
      Cairo_gtk.create graphical_view#misc#window

    let size () =
      let {Gtk.width; height; _} = graphical_view#misc#allocation in
      (width, height)

    let on_refresh_needed f =
      graphical_view#event#connect#expose ~callback:(fun _ -> f (); true)
      |> ignore
  end

  module Timer = struct
    let set_recurring ~seconds f =
      Glib.Timeout.add ~ms:(Int.of_float (1000. *. seconds)) ~callback:(fun _ -> f (); true)
      |> ignore
  end

  module Toolbar = struct
    let on_save_clicked f =
      save_button#connect#clicked ~callback:(fun _ -> f ())
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
