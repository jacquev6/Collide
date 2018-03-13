open General.Abbr
open Collide_

module OCSR = OCamlStandard.Random

let () = Exn.record_backtraces true

module Drawer = Drawer.Make(Cairo)

let draw_frame ?event format =
  Frmt.with_result format ~f:(fun file_name simulation ->
    let (w, h) = Drawer.measure simulation in
    let image = Cairo.Image.(create RGB24 ~width:(Int.of_float w) ~height:(Int.of_float h)) in
    let context = Cairo.create image in
    Cairo.set_source_rgb context ~r:0.9 ~g:0.9 ~b:1.;
    Cairo.paint context;
    Drawer.draw ~context ?event simulation;
    Cairo.PNG.write image file_name
  )

let () = begin
  let () = OCSR.init 42 in
  let simulation = Simulation.(
    let w = 640. and h = 480. in
    create
    ~dimensions:(w, h)
    (
      IntRa.make 30
      |> IntRa.ToList.map ~f:(fun _ ->
        let rd a b = a +. OCSR.float (b -. a) in
        let radius = rd 3. 15. in
        Ball.{
          radius;
          density=(rd 0.1 1.);
          position=((rd radius (w -. radius)), (rd radius (h -. radius)));
          velocity=(let v_max = 100. in ((rd (-.v_max) v_max), (rd (-.v_max) v_max)));
        };
      )
    )
  ) in
  let images_per_second = Int.of_string OCamlStandard.Sys.argv.(1)
  and duration = Int.of_string OCamlStandard.Sys.argv.(2) in
  let frames = duration * images_per_second + 1 in
  assert (frames <= 99999999); (* Because we need zero-padded filenames for ffmpeg and use %08d below. *)
  IntRa.make frames
  |> IntRa.fold ~init:simulation ~f:(fun simulation i ->
    let max_date = Fl.of_int i /. Fl.of_int images_per_second in
    let rec aux simulation =
      let (event, simulation) = Simulation.advance simulation ~max_date in
      match event with
        | None -> draw_frame "%08d.png" i simulation; simulation
        | Some event -> draw_frame ~event "%06.2f.png" (Simulation.date simulation) simulation; aux simulation
    in
    aux simulation
  )
  |> ignore
end
