open General.Abbr


module Make(C: JsOfOCairo.S) = struct
  let draw_ball ~context ~display_velocity_vectors ~alpha {Simulation.Ball.radius; density; position=(x, y); velocity=(vx, vy)} =
    C.save context;
    let rgb = 0.7 *. (1. -. density) in
    C.set_source_rgba context ~r:rgb ~g:rgb ~b:rgb ~a:alpha;
    C.arc context ~x ~y ~r:radius ~a1:0. ~a2:(2. *. Fl.pi);
    C.fill context;
    if display_velocity_vectors then begin
      C.set_source_rgba context ~r:1. ~g:0. ~b:0. ~a:alpha;
      C.move_to context ~x ~y;
      C.rel_line_to context ~x:vx ~y:vy;
      C.save context;
      C.translate context ~x:(x +. vx) ~y:(y +. vy);
      C.rotate context ~angle:(Fl.atan2 ~x:vx ~y:vy);
      C.move_to context ~x:(-10.) ~y:(-10.);
      C.line_to context ~x:0. ~y:0.;
      C.line_to context ~x:(-10.) ~y:10.;
      C.restore context;
      C.stroke context;
    end;
    C.restore context

  let draw_balls ~context ~display_velocity_vectors ~alpha simulation =
    simulation
    |> Simulation.balls
    |> Li.iter ~f:(draw_ball ~context ~display_velocity_vectors ~alpha)

  let draw ~context ~display_velocity_vectors ?(alpha=1.) simulation =
    C.save context;
    draw_balls ~context ~display_velocity_vectors ~alpha simulation;
    C.restore context
end
