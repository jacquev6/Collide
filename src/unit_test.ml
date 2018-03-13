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
            {Ball.radius=10.; density=1.; position=(40., 60.); velocity=(1., 2.)}
            []
            {Ball.radius=10.; density=1.; position=(42., 64.); velocity=(1., 2.)};
          make "start from left" 1.
            {Ball.radius=10.; density=1.; position=(10., 150.); velocity=(1., 0.)}
            []
            {Ball.radius=10.; density=1.; position=(11., 150.); velocity=(1., 0.)};
          make "start from top" 1.
            {Ball.radius=10.; density=1.; position=(200., 10.); velocity=(0., 1.)}
            []
            {Ball.radius=10.; density=1.; position=(200., 11.); velocity=(0., 1.)};
          make "start from right" 1.
            {Ball.radius=10.; density=1.; position=(390., 150.); velocity=(-1., 0.)}
            []
            {Ball.radius=10.; density=1.; position=(389., 150.); velocity=(-1., 0.)};
          make "start from bottom" 1.
            {Ball.radius=10.; density=1.; position=(200., 290.); velocity=(0., -1.)}
            []
            {Ball.radius=10.; density=1.; position=(200., 289.); velocity=(0., -1.)};
          make "start to left" 1.
            {Ball.radius=10.; density=1.; position=(10., 150.); velocity=(-1., 0.)}
            [
              (0., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 150.); velocity=(-1., 0.)};
                after={radius=10.; density=1.; position=(10., 150.); velocity=(1., 0.)};
              });
            ]
            {Ball.radius=10.; density=1.; position=(11., 150.); velocity=(1., 0.)};
          make "start to top" 1.
            {Ball.radius=10.; density=1.; position=(200., 10.); velocity=(0., -1.)}
            [
              (0., WallBallCollision {
                wall=Top;
                before={radius=10.; density=1.; position=(200., 10.); velocity=(0., -1.)};
                after={radius=10.; density=1.; position=(200., 10.); velocity=(0., 1.)};
              });
            ]
            {Ball.radius=10.; density=1.; position=(200., 11.); velocity=(0., 1.)};
          make "start to right" 1.
            {Ball.radius=10.; density=1.; position=(390., 150.); velocity=(1., 0.)}
            [
              (0., WallBallCollision {
                wall=Right;
                before={radius=10.; density=1.; position=(390., 150.); velocity=(1., 0.)};
                after={radius=10.; density=1.; position=(390., 150.); velocity=(-1., 0.)};
              });
            ]
            {Ball.radius=10.; density=1.; position=(389., 150.); velocity=(-1., 0.)};
          make "start to bottom" 1.
            {Ball.radius=10.; density=1.; position=(200., 290.); velocity=(0., 1.)}
            [
              (0., WallBallCollision {
                wall=Bottom;
                before={radius=10.; density=1.; position=(200., 290.); velocity=(0., 1.)};
                after={radius=10.; density=1.; position=(200., 290.); velocity=(0., -1.)};
              });
            ]
            {Ball.radius=10.; density=1.; position=(200., 289.); velocity=(0., -1.)};
          make "hit left wall" 2.
            {Ball.radius=10.; density=1.; position=(11., 60.); velocity=(-1., 2.)}
            [
              (1., WallBallCollision {
                wall=Left;
                before={Ball.radius=10.; density=1.; position=(10., 62.); velocity=(-1., 2.)};
                after={Ball.radius=10.; density=1.; position=(10., 62.); velocity=(1., 2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(11., 64.); velocity=(1., 2.)};
          make "hit right wall" 2.
            {Ball.radius=10.; density=1.; position=(389., 60.); velocity=(1., 2.)}
            [
              (1., WallBallCollision {
                wall=Right;
                before={Ball.radius=10.; density=1.; position=(390., 62.); velocity=(1., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 62.); velocity=(-1., 2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(389., 64.); velocity=(-1., 2.)};
          make "hit top wall" 2.
            {Ball.radius=10.; density=1.; position=(100., 12.); velocity=(1., -2.)}
            [
              (1., WallBallCollision {
                wall=Top;
                before={Ball.radius=10.; density=1.; position=(101., 10.); velocity=(1., -2.)};
                after={Ball.radius=10.; density=1.; position=(101., 10.); velocity=(1., 2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(102., 12.); velocity=(1., 2.)};
          make "hit bottom wall" 2.
            {Ball.radius=10.; density=1.; position=(100., 288.); velocity=(1., 2.)}
            [
              (1., WallBallCollision {
                wall=Bottom;
                before={Ball.radius=10.; density=1.; position=(101., 290.); velocity=(1., 2.)};
                after={Ball.radius=10.; density=1.; position=(101., 290.); velocity=(1., -2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(102., 288.); velocity=(1., -2.)};
          make "hit corner" 2.
            {Ball.radius=10.; density=1.; position=(389., 288.); velocity=(1., 2.)}
            [
              (1., WallBallCollision {
                wall=Right;
                before={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(1., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-1., 2.)}
              });
              (1., WallBallCollision {
                wall=Bottom;
                before={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-1., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-1., -2.)}
              });
            ]
            {Ball.radius=10.; density=1.; position=(389., 288.); velocity=(-1., -2.)};
          make "hit wall after corner" 40.
            {Ball.radius=10.; density=1.; position=(380., 288.); velocity=(10., 2.)}
            [
              (1., WallBallCollision {
                wall=Right;
                before={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(10., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-10., 2.)};
              });
              (1., WallBallCollision {
                wall=Bottom;
                before={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-10., 2.)};
                after={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-10., -2.)};
              });
              (39., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 214.); velocity=(-10., -2.)};
                after={radius=10.; density=1.; position=(10., 214.); velocity=(10., -2.)};
              });
            ]
            {Ball.radius=10.; density=1.; position=(20., 212.); velocity=(10., -2.)};
          make "hit corners" 80.
            {Ball.radius=10.; density=1.; position=(48., 38.); velocity=(38., 28.)}
            [
              (9., WallBallCollision {
                wall=Right;
                before={radius=10.; density=1.; position=(390., 290.); velocity=(38., 28.)};
                after={radius=10.; density=1.; position=(390., 290.); velocity=(-38., 28.)};
              });
              (9., WallBallCollision {
                wall=Bottom;
                before={radius=10.; density=1.; position=(390., 290.); velocity=(-38., 28.)};
                after={radius=10.; density=1.; position=(390., 290.); velocity=(-38., -28.)};
              });
              (19., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 10.); velocity=(-38., -28.)};
                after={radius=10.; density=1.; position=(10., 10.); velocity=(38., -28.)};
              });
              (19., WallBallCollision {
                wall=Top;
                before={radius=10.; density=1.; position=(10., 10.); velocity=(38., -28.)};
                after={radius=10.; density=1.; position=(10., 10.); velocity=(38., 28.)};
              });
              (29., WallBallCollision {
                wall=Bottom;
                before={radius=10.; density=1.; position=(390., 290.); velocity=(38., 28.)};
                after={radius=10.; density=1.; position=(390., 290.); velocity=(38., -28.)};
              });
              (29., WallBallCollision {
                wall=Right;
                before={radius=10.; density=1.; position=(390., 290.); velocity=(38., -28.)};
                after={radius=10.; density=1.; position=(390., 290.); velocity=(-38., -28.)};
              });
              (39., WallBallCollision {
                wall=Top;
                before={radius=10.; density=1.; position=(10., 10.); velocity=(-38., -28.)};
                after={radius=10.; density=1.; position=(10., 10.); velocity=(-38., 28.)};
              });
              (39., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 10.); velocity=(-38., 28.)};
                after={radius=10.; density=1.; position=(10., 10.); velocity=(38., 28.)};
              });
              (49., WallBallCollision {
                wall=Bottom;
                before={radius=10.; density=1.; position=(390., 290.); velocity=(38., 28.)};
                after={radius=10.; density=1.; position=(390., 290.); velocity=(38., -28.)};
              });
              (49., WallBallCollision {
                wall=Right;
                before={radius=10.; density=1.; position=(390., 290.); velocity=(38., -28.)};
                after={radius=10.; density=1.; position=(390., 290.); velocity=(-38., -28.)};
              });
              (59., WallBallCollision {
                wall=Top;
                before={radius=10.; density=1.; position=(10., 10.); velocity=(-38., -28.)};
                after={radius=10.; density=1.; position=(10., 10.); velocity=(-38., 28.)};
              });
              (59., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 10.); velocity=(-38., 28.)};
                after={radius=10.; density=1.; position=(10., 10.); velocity=(38., 28.)};
              });
              (69., WallBallCollision {
                wall=Bottom;
                before={radius=10.; density=1.; position=(390., 290.); velocity=(38., 28.)};
                after={radius=10.; density=1.; position=(390., 290.); velocity=(38., -28.)};
              });
              (69., WallBallCollision {
                wall=Right;
                before={radius=10.; density=1.; position=(390., 290.); velocity=(38., -28.)};
                after={radius=10.; density=1.; position=(390., 290.); velocity=(-38., -28.)};
              });
              (79., WallBallCollision {
                wall=Top;
                before={radius=10.; density=1.; position=(10., 10.); velocity=(-38., -28.)};
                after={radius=10.; density=1.; position=(10., 10.); velocity=(-38., 28.)};
              });
              (79., WallBallCollision {
                wall=Left;
                before={radius=10.; density=1.; position=(10., 10.); velocity=(-38., 28.)};
                after={radius=10.; density=1.; position=(10., 10.); velocity=(38., 28.)};
              });
            ]
            {Ball.radius=10.; density=1.; position=(48., 38.); velocity=(38., 28.)};
        ]
      );
    ];
  ]
end

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv T.test)