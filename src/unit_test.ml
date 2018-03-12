open General.Abbr
open Tst
open Collide_

module T = struct
  let test = "Collide" >:: [
    "Simulation" >:: Simulation.[
      "Single-ball" >:: (
        let make name max_date initial_ball expected_events expected_ball =
          name >: (lazy (
            let simulation = create ~dimensions:(400., 300.) [initial_ball] in
            let simulation = 
              expected_events
              |> Li.fold ~init:simulation ~f:(fun simulation (expected_date, expected_event) ->
                let (actual_event, simulation) = advance ~max_date simulation in
                check_some_poly ~repr:Event.repr ~expected:expected_event actual_event;
                check_float ~expected:expected_date (date simulation);
                simulation
              )
            in
            let (actual_event, simulation) = advance ~max_date simulation in
            check_none_poly ~repr:Event.repr actual_event;
            check_float ~expected:max_date (date simulation);
            check_list_poly ~repr:Ball.repr ~expected:[expected_ball] (balls simulation)
          ))
        in
        [
          make "advance" 2.
            {Ball.radius=10.; density=1.; position=(40., 60.); speed=(1., 2.)}
            []
            {Ball.radius=10.; density=1.; position=(42., 64.); speed=(1., 2.)};
          make "hit left wall" 2.
            {Ball.radius=10.; density=1.; position=(11., 60.); speed=(-1., 2.)}
            [
              (1., Event.WallBallCollision {
                wall=Left;
                before={Ball.radius=10.; density=1.; position=(10., 62.); speed=(-1., 2.)};
                after={Ball.radius=10.; density=1.; position=(10., 62.); speed=(1., 2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(11., 64.); speed=(1., 2.)};
          make "hit right wall" 2.
            {Ball.radius=10.; density=1.; position=(389., 60.); speed=(1., 2.)}
            [
              (1., Event.WallBallCollision {
                wall=Right;
                before={Ball.radius=10.; density=1.; position=(390., 62.); speed=(1., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 62.); speed=(-1., 2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(389., 64.); speed=(-1., 2.)};
          make "hit top wall" 2.
            {Ball.radius=10.; density=1.; position=(100., 12.); speed=(1., -2.)}
            [
              (1., Event.WallBallCollision {
                wall=Top;
                before={Ball.radius=10.; density=1.; position=(101., 10.); speed=(1., -2.)};
                after={Ball.radius=10.; density=1.; position=(101., 10.); speed=(1., 2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(102., 12.); speed=(1., 2.)};
          make "hit bottom wall" 2.
            {Ball.radius=10.; density=1.; position=(100., 288.); speed=(1., 2.)}
            [
              (1., Event.WallBallCollision {
                wall=Bottom;
                before={Ball.radius=10.; density=1.; position=(101., 290.); speed=(1., 2.)};
                after={Ball.radius=10.; density=1.; position=(101., 290.); speed=(1., -2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(102., 288.); speed=(1., -2.)};
          make "hit corner" 2.
            (* @todo After this test, two identical events are scheduled (happens_at=381.). Find a way to cancel one of them. Run simulation until then and ensure things go well. *)
            {Ball.radius=10.; density=1.; position=(389., 288.); speed=(1., 2.)}
            [
              (1., Event.WallBallCollision {
                wall=Right;
                before={Ball.radius=10.; density=1.; position=(390., 290.); speed=(1., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 290.); speed=(-1., 2.)}
              });
              (1., Event.WallBallCollision {
                wall=Bottom;
                before={Ball.radius=10.; density=1.; position=(390., 290.); speed=(-1., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 290.); speed=(-1., -2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(389., 288.); speed=(-1., -2.)};
        ]
      );
    ];
  ]
end

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv T.test)
