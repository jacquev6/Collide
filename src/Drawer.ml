open General.Abbr


module Make(C: JsOfOCairo.S) = struct
  module Settings = struct
    type t = {
      draw_velocity: bool;
    }
  end

  let draw_ball ~context ~settings:{Settings.draw_velocity} {Simulation.Ball.radius; density; position=(x, y); velocity=(vx, vy)} =
    C.save context;
    let rgb = 0.7 *. (1. -. density) in
    C.set_source_rgb context ~r:rgb ~g:rgb ~b:rgb;
    C.arc context ~x ~y ~r:radius ~a1:0. ~a2:(2. *. Fl.pi);
    C.fill context;
    if draw_velocity then begin
      C.set_source_rgb context ~r:1. ~g:0. ~b:0.;
      C.move_to context ~x ~y;
      C.rel_line_to context ~x:(vx /. 10.) ~y:(vy /. 10.);
      C.stroke context;
    end;
    C.restore context

  let draw_balls ~context ~settings simulation =
    simulation
    |> Simulation.balls
    |> Li.iter ~f:(draw_ball ~context ~settings)

  let draw ~context ~settings simulation =
    C.save context;
    C.set_source_rgb context ~r:1. ~g:1. ~b:1.;
    C.paint context;
    draw_balls ~context ~settings simulation;
    C.restore context
end
