open General.Abbr


module Make(C: JsOfOCairo.S) = struct
  let draw_ball ~context {Simulation.Ball.radius; density; position=(x, y); speed=(sx, sy)} =
    C.save context;
    let rgb = 0.7 *. (1. -. density) in
    C.set_source_rgb context ~r:rgb ~g:rgb ~b:rgb;
    C.arc context ~x ~y ~r:radius ~a1:0. ~a2:(2. *. Fl.pi);
    C.fill context;
    C.set_source_rgb context ~r:1. ~g:0. ~b:0.;
    C.move_to context ~x ~y;
    C.rel_line_to context ~x:(10. *. sx) ~y:(10. *. sy);
    C.stroke context;
    C.restore context

  let draw ~context simulation =
    simulation
    |> Simulation.balls
    |> Li.iter ~f:(draw_ball ~context)
end
