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
          (* @todo Start with ball touching wall and/or corner *)
          make "advance" 2.
            {Ball.radius=10.; density=1.; position=(40., 60.); speed=(1., 2.)}
            []
            {Ball.radius=10.; density=1.; position=(42., 64.); speed=(1., 2.)};
          make "hit left wall" 2.
            {Ball.radius=10.; density=1.; position=(11., 60.); speed=(-1., 2.)}
            [
              (1., WallBallCollision {
                wall=Left;
                before={Ball.radius=10.; density=1.; position=(10., 62.); speed=(-1., 2.)};
                after={Ball.radius=10.; density=1.; position=(10., 62.); speed=(1., 2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(11., 64.); speed=(1., 2.)};
          make "hit right wall" 2.
            {Ball.radius=10.; density=1.; position=(389., 60.); speed=(1., 2.)}
            [
              (1., WallBallCollision {
                wall=Right;
                before={Ball.radius=10.; density=1.; position=(390., 62.); speed=(1., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 62.); speed=(-1., 2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(389., 64.); speed=(-1., 2.)};
          make "hit top wall" 2.
            {Ball.radius=10.; density=1.; position=(100., 12.); speed=(1., -2.)}
            [
              (1., WallBallCollision {
                wall=Top;
                before={Ball.radius=10.; density=1.; position=(101., 10.); speed=(1., -2.)};
                after={Ball.radius=10.; density=1.; position=(101., 10.); speed=(1., 2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(102., 12.); speed=(1., 2.)};
          make "hit bottom wall" 2.
            {Ball.radius=10.; density=1.; position=(100., 288.); speed=(1., 2.)}
            [
              (1., WallBallCollision {
                wall=Bottom;
                before={Ball.radius=10.; density=1.; position=(101., 290.); speed=(1., 2.)};
                after={Ball.radius=10.; density=1.; position=(101., 290.); speed=(1., -2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(102., 288.); speed=(1., -2.)};
          make "hit corner" 2.
            {Ball.radius=10.; density=1.; position=(389., 288.); speed=(1., 2.)}
            [
              (1., WallBallCollision {
                wall=Right;
                before={Ball.radius=10.; density=1.; position=(390., 290.); speed=(1., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 290.); speed=(-1., 2.)}
              });
              (1., WallBallCollision {
                wall=Bottom;
                before={Ball.radius=10.; density=1.; position=(390., 290.); speed=(-1., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 290.); speed=(-1., -2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(389., 288.); speed=(-1., -2.)};
          make "hit wall after corner" 40.
            {Ball.radius=10.; density=1.; position=(380., 288.); speed=(10., 2.)}
            [
              (1., WallBallCollision {
                wall=Right;
                before={Ball.radius=10.; density=1.; position=(390., 290.); speed=(10., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 290.); speed=(-10., 2.)};
              });
              (1., WallBallCollision {
                wall=Bottom;
                before={Ball.radius=10.; density=1.; position=(390., 290.); speed=(-10., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 290.); speed=(-10., -2.)};
              });
              (39., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 214.); speed=(-10., -2.)};
                after={radius=10.; density=1.; position=(10., 214.); speed=(10., -2.)};
              });
            ]
            {Ball.radius=10.; density=1.; position=(20., 212.); speed=(10., -2.)};
          make "hit corners" 80.
            {Ball.radius=10.; density=1.; position=(48., 38.); speed=(38., 28.)}
            [
              (9., WallBallCollision {
                wall=Right;
                before={radius=10.; density=1.; position=(390., 290.); speed=(38., 28.)};
                after={radius=10.; density=1.; position=(390., 290.); speed=(-38., 28.)};
              });
              (9., WallBallCollision {
                wall=Bottom;
                before={radius=10.; density=1.; position=(390., 290.); speed=(-38., 28.)};
                after={radius=10.; density=1.; position=(390., 290.); speed=(-38., -28.)};
              });
              (19., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 10.); speed=(-38., -28.)};
                after={radius=10.; density=1.; position=(10., 10.); speed=(38., -28.)};
              });
              (19., WallBallCollision {
                wall=Top;
                before={radius=10.; density=1.; position=(10., 10.); speed=(38., -28.)};
                after={radius=10.; density=1.; position=(10., 10.); speed=(38., 28.)};
              });
              (29., WallBallCollision {
                wall=Bottom;
                before={radius=10.; density=1.; position=(390., 290.); speed=(38., 28.)};
                after={radius=10.; density=1.; position=(390., 290.); speed=(38., -28.)};
              });
              (29., WallBallCollision {
                wall=Right;
                before={radius=10.; density=1.; position=(390., 290.); speed=(38., -28.)};
                after={radius=10.; density=1.; position=(390., 290.); speed=(-38., -28.)};
              });
              (39., WallBallCollision {
                wall=Top;
                before={radius=10.; density=1.; position=(10., 10.); speed=(-38., -28.)};
                after={radius=10.; density=1.; position=(10., 10.); speed=(-38., 28.)};
              });
              (39., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 10.); speed=(-38., 28.)};
                after={radius=10.; density=1.; position=(10., 10.); speed=(38., 28.)};
              });
              (49., WallBallCollision {
                wall=Bottom;
                before={radius=10.; density=1.; position=(390., 290.); speed=(38., 28.)};
                after={radius=10.; density=1.; position=(390., 290.); speed=(38., -28.)};
              });
              (49., WallBallCollision {
                wall=Right;
                before={radius=10.; density=1.; position=(390., 290.); speed=(38., -28.)};
                after={radius=10.; density=1.; position=(390., 290.); speed=(-38., -28.)};
              });
              (59., WallBallCollision {
                wall=Top;
                before={radius=10.; density=1.; position=(10., 10.); speed=(-38., -28.)};
                after={radius=10.; density=1.; position=(10., 10.); speed=(-38., 28.)};
              });
              (59., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 10.); speed=(-38., 28.)};
                after={radius=10.; density=1.; position=(10., 10.); speed=(38., 28.)};
              });
              (69., WallBallCollision {
                wall=Bottom;
                before={radius=10.; density=1.; position=(390., 290.); speed=(38., 28.)};
                after={radius=10.; density=1.; position=(390., 290.); speed=(38., -28.)};
              });
              (69., WallBallCollision {
                wall=Right;
                before={radius=10.; density=1.; position=(390., 290.); speed=(38., -28.)};
                after={radius=10.; density=1.; position=(390., 290.); speed=(-38., -28.)};
              });
              (79., WallBallCollision {
                wall=Top;
                before={radius=10.; density=1.; position=(10., 10.); speed=(-38., -28.)};
                after={radius=10.; density=1.; position=(10., 10.); speed=(-38., 28.)};
              });
              (79., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 10.); speed=(-38., 28.)};
                after={radius=10.; density=1.; position=(10., 10.); speed=(38., 28.)};
              });
            ]
            {Ball.radius=10.; density=1.; position=(48., 38.); speed=(38., 28.)};
        ]
      );
    ];
  ]
end

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv T.test)
