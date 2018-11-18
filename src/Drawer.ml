open General.Abbr


module Make(C: JsOfOCairo.S) = struct
  let draw_ball ~context ~velocity_vectors ~alpha {Simulation.Ball.radius; density; position=(x, y); velocity=(vx, vy)} =
    C.save context;
    let rgb = 0.7 *. (1. -. density) in
    C.set_source_rgba context rgb rgb rgb alpha;
    C.arc context x y ~r:radius ~a1:0. ~a2:(2. *. Fl.pi);
    C.fill context;
    if velocity_vectors then begin
      C.set_source_rgba context 1. 0. 0. alpha;
      C.move_to context x y;
      C.rel_line_to context vx vy;
      C.save context;
      C.translate context (x +. vx) (y +. vy);
      C.rotate context (Fl.atan2 ~x:vx ~y:vy);
      C.move_to context (-10.) (-10.);
      C.line_to context 0. 0.;
      C.line_to context (-10.) 10.;
      C.restore context;
      C.stroke context;
    end;
    C.restore context

  let draw_balls ~context ~velocity_vectors ~alpha simulation =
    simulation
    |> Simulation.balls
    |> Li.iter ~f:(draw_ball ~context ~velocity_vectors ~alpha)

  let draw ~context ~velocity_vectors ?(alpha=1.) simulation =
    C.save context;
    draw_balls ~context ~velocity_vectors ~alpha simulation;
    C.restore context
end
