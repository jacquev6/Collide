open General.Abbr


module Make(C: JsOfOCairo.S) = struct
  let draw_ball ~context {Simulation.Ball.radius; density; position=(x, y); velocity=(vx, vy)} =
    C.save context;
    let rgb = 0.7 *. (1. -. density) in
    C.set_source_rgb context ~r:rgb ~g:rgb ~b:rgb;
    C.arc context ~x ~y ~r:radius ~a1:0. ~a2:(2. *. Fl.pi);
    C.fill context;
    C.set_source_rgb context ~r:1. ~g:0. ~b:0.;
    C.move_to context ~x ~y;
    C.rel_line_to context ~x:(vx /. 10.) ~y:(vy /. 10.);
    C.stroke context;
    C.restore context

  let draw_balls ~context simulation =
    simulation
    |> Simulation.balls
    |> Li.iter ~f:(draw_ball ~context)

  let draw_event ~context = function
    | Simulation.Event.BallBallCollision _ ->
      (* @todo Implement *)
      ()
    | Simulation.Event.WallBallCollision {wall=_; before={Simulation.Ball.velocity=(vx, vy); _}; after={Simulation.Ball.position=(x, y); _}} -> begin
      C.save context;
      C.set_line_width context 1.;
      C.move_to context ~x ~y;
      C.set_source_rgb context ~r:1. ~g:0. ~b:0.;
      C.rel_line_to context ~x:(vx /. 10.) ~y:(vy /. 10.);
      C.stroke context;
      C.restore context
    end

  let draw ~context ?event simulation =
    C.save context;
    C.set_source_rgb context ~r:1. ~g:1. ~b:1.;
    C.paint context;
    draw_balls ~context simulation;
    Opt.iter ~f:(draw_event ~context) event;
    C.restore context
end
