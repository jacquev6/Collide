open General.Abbr
open Tst
open Collide

module T = struct
  let test = "Collide" >:: [
    "Simulation" >:: Simulation.(
      let make name max_date initial_balls expected_events expected_balls =
        name >: (lazy (
          let initial_simulation = create ~dimensions:(400., 300.) initial_balls in
          let (actual_events, final_simulation) = advance ~date:max_date initial_simulation in
          check_float ~expected:max_date (date final_simulation);
          check_list_poly ~repr:Ball.repr ~expected:expected_balls (balls final_simulation);
          let repr (date, event) =
            Frmt.apply "(%f, %s)" date (Event.repr event) (*BISECT-IGNORE*)
          in
          check_list_poly ~repr ~expected:expected_events actual_events
        ))
      in
      [
        "Single-ball" >:: (
          let make name max_date initial_ball expected_events expected_ball =
            make name max_date [initial_ball] expected_events [expected_ball]
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
            make "start to bottom-right" 2.
              {Ball.radius=10.; density=1.; position=(389., 289.); velocity=(1., 1.)}
              [
                (1., WallBallCollision {
                  wall=Right;
                  before={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(1., 1.)};
                  after={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-1., 1.)}
                });
                (1., WallBallCollision {
                  wall=Bottom;
                  before={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-1., 1.)};
                  after={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-1., -1.)}
                });
              ]
              {Ball.radius=10.; density=1.; position=(389., 289.); velocity=(-1., -1.)};
            make "start at bottom-right" 1.
              {Ball.radius=10.; density=1.; position=(390., 290.); velocity=(1., 1.)}
              [
                (0., WallBallCollision {
                  wall=Right;
                  before={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(1., 1.)};
                  after={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-1., 1.)}
                });
                (0., WallBallCollision {
                  wall=Bottom;
                  before={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-1., 1.)};
                  after={Ball.radius=10.; density=1.; position=(390., 290.); velocity=(-1., -1.)}
                });
              ]
              {Ball.radius=10.; density=1.; position=(389., 289.); velocity=(-1., -1.)};
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
        "Dual-ball" >:: (
          let make name max_date initial_ball_1 initial_ball_2 expected_events expected_ball_1 expected_ball_2 =
            make name max_date [initial_ball_1; initial_ball_2] expected_events [expected_ball_1; expected_ball_2]
          in
          [
            make "no collision of balls going in same direction" 2.
              {Ball.radius=10.; density=1.; position=(40., 100.); velocity=(1., 0.)}
              {Ball.radius=10.; density=1.; position=(61., 100.); velocity=(1., 0.)}
              []
              {Ball.radius=10.; density=1.; position=(42., 100.); velocity=(1., 0.)}
              {Ball.radius=10.; density=1.; position=(63., 100.); velocity=(1., 0.)};
            make "no collision of balls going in same direction" 2.
              {Ball.radius=10.; density=1.; position=(40., 100.); velocity=(1., 0.)}
              {Ball.radius=10.; density=1.; position=(61., 100.); velocity=(2., 0.)}
              []
              {Ball.radius=10.; density=1.; position=(42., 100.); velocity=(1., 0.)}
              {Ball.radius=10.; density=1.; position=(65., 100.); velocity=(2., 0.)};
            make "balls staying in contact" 2.
              {Ball.radius=10.; density=1.; position=(40., 100.); velocity=(1., 0.)}
              {Ball.radius=10.; density=1.; position=(60., 100.); velocity=(1., 0.)}
              []
              {Ball.radius=10.; density=1.; position=(42., 100.); velocity=(1., 0.)}
              {Ball.radius=10.; density=1.; position=(62., 100.); velocity=(1., 0.)};
            make "frontal horizontal collision of identical balls" 2.
              {Ball.radius=10.; density=1.; position=(40., 100.); velocity=(1., 0.)}
              {Ball.radius=10.; density=1.; position=(61., 100.); velocity=(0., 0.)}
              [
                (1., BallBallCollision {
                  before=(
                    {radius=10.; density=1.; position=(41., 100.); velocity=(1., 0.)},
                    {radius=10.; density=1.; position=(61., 100.); velocity=(0., 0.)}
                  );
                  after=(
                    {radius=10.; density=1.; position=(41., 100.); velocity=(0., 0.)},
                    {radius=10.; density=1.; position=(61., 100.); velocity=(1., 0.)}
                  );
                });
              ]
              {Ball.radius=10.; density=1.; position=(41., 100.); velocity=(0., 0.)}
              {Ball.radius=10.; density=1.; position=(62., 100.); velocity=(1., 0.)};
            make "frontal horizontal collision of identical balls" 2.
              {Ball.radius=10.; density=1.; position=(39., 100.); velocity=(1., 0.)}
              {Ball.radius=10.; density=1.; position=(61., 100.); velocity=(-1., 0.)}
              [
                (1., BallBallCollision {
                  before=(
                    {radius=10.; density=1.; position=(40., 100.); velocity=(1., 0.)},
                    {radius=10.; density=1.; position=(60., 100.); velocity=(-1., 0.)}
                  );
                  after=(
                    {radius=10.; density=1.; position=(40., 100.); velocity=(-1., 0.)},
                    {radius=10.; density=1.; position=(60., 100.); velocity=(1., 0.)}
                  );
                });
              ]
              {Ball.radius=10.; density=1.; position=(39., 100.); velocity=(-1., 0.)}
              {Ball.radius=10.; density=1.; position=(61., 100.); velocity=(1., 0.)};
            make "frontal horizontal collision of identical balls" 1.
              {Ball.radius=10.; density=1.; position=(40., 100.); velocity=(1., 0.)}
              {Ball.radius=10.; density=1.; position=(60., 100.); velocity=(-1., 0.)}
              [
                (0., BallBallCollision {
                  before=(
                    {radius=10.; density=1.; position=(40., 100.); velocity=(1., 0.)},
                    {radius=10.; density=1.; position=(60., 100.); velocity=(-1., 0.)}
                  );
                  after=(
                    {radius=10.; density=1.; position=(40., 100.); velocity=(-1., 0.)},
                    {radius=10.; density=1.; position=(60., 100.); velocity=(1., 0.)}
                  );
                });
              ]
              {Ball.radius=10.; density=1.; position=(39., 100.); velocity=(-1., 0.)}
              {Ball.radius=10.; density=1.; position=(61., 100.); velocity=(1., 0.)};
            make "frontal horizontal collision of different balls of same mass" 2.
              {Ball.radius=10.; density=1.; position=(40., 100.); velocity=(1., 0.)}
              {Ball.radius=100.; density=0.01; position=(151., 100.); velocity=(0., 0.)}
              [
                (1., BallBallCollision {
                  before=(
                    {radius=10.; density=1.; position=(41., 100.); velocity=(1., 0.)},
                    {radius=100.; density=0.01; position=(151., 100.); velocity=(0., 0.)}
                  );
                  after=(
                    {radius=10.; density=1.; position=(41., 100.); velocity=(0., 0.)},
                    {radius=100.; density=0.01; position=(151., 100.); velocity=(1., 0.)}
                  );
                });
              ]
              {Ball.radius=10.; density=1.; position=(41., 100.); velocity=(0., 0.)}
              {Ball.radius=100.; density=0.01; position=(152., 100.); velocity=(1., 0.)};
            make "frontal horizontal collision of different balls of different mass" 2.
              {Ball.radius=10.; density=1.; position=(40., 100.); velocity=(1., 0.)}
              {Ball.radius=10.; density=9.; position=(61., 100.); velocity=(0., 0.)}
              [
                (1., BallBallCollision {
                  before=(
                    {radius=10.; density=1.; position=(41., 100.); velocity=(1., 0.)},
                    {radius=10.; density=9.; position=(61., 100.); velocity=(0., 0.)}
                  );
                  after=(
                    {radius=10.; density=1.; position=(41., 100.); velocity=(-0.80, 0.)},
                    {radius=10.; density=9.; position=(61., 100.); velocity=(0.20, 0.)}
                  );
                });
              ]
              {Ball.radius=10.; density=1.; position=(40.20, 100.); velocity=(-0.80, 0.)}
              {Ball.radius=10.; density=9.; position=(61.20, 100.); velocity=(0.20, 0.)};
          ]
        );
      ]
    );
  ]
end

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv T.test)
