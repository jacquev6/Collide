open General.Abbr
module OCSA = OCamlStandard.ArrayLabels

module Public = struct
  module Wall = struct
    type t =
      | Left
      | Right
      | Top
      | Bottom
  end

  module Ball = struct
    type t = {
      radius: float;
      density: float;
      position: float * float;
      speed: float * float;
    }
  end

  module Event = struct
    type t =
      | BallBallCollision of {
        before: Ball.t * Ball.t;
        after: Ball.t * Ball.t;
      }
      | BallWallCollision of {
        wall: Wall.t;
        before: Ball.t;
        after: Ball.t;
      }
  end
end

module Internal = struct
  module Ball = struct
    type t = {
      radius: float;
      density: float;
      date: float;
      position: float * float;
      speed: float * float;
    }

    let of_public ~date {Public.Ball.radius; density; position; speed} =
      {radius; density; date; position; speed}

    let position ~date {date=t0; position=(x, y); speed=(sx, sy); _} =
      assert (date >= t0);
      let dt = date -. t0 in
      let x = x +. dt *. sx
      and y = y +. dt *. sy in
      (x, y)

    let to_public ~date ({radius; density; speed; _} as ball) =
      let position = position ~date ball in
      {Public.Ball.radius; density; position; speed}
  end

  module Event = struct
    module Wall = struct
      type dir =
        | Horizontal
        | Vertical

      type loc =
        | Low
        | High

      type t = dir * loc

      let to_public = function
        | (Horizontal, Low) -> Public.Wall.Top
        | (Horizontal, High) -> Public.Wall.Bottom
        | (Vertical, Low) -> Public.Wall.Left
        | (Vertical, High) -> Public.Wall.Right

      let next ~direction ~dimensions {Ball.radius; density=_; date; position; speed} =
        let getter =
          match direction with
            | Horizontal -> Tu2.get_1
            | Vertical -> Tu2.get_0
        in
        let dimension = getter dimensions
        and position = getter position
        and speed = getter speed in
        assert (0. +. radius <= position);
        assert (position <= dimension -. radius);
        if speed > 0. then
          Some (date +. ((dimension -. radius) -. position) /. speed, (direction, High))
        else if speed < 0. then
          Some (date +. (position -. (0. +. radius)) /. speed, (direction, Low))
        else
          None

      let apply ~date ~ball wall =
        let position = Ball.position ~date ball
        and speed =
          let (sx, sy) = ball.Ball.speed in
          match wall with
            | (Horizontal, _) -> (sx, -.sy)
            | (Vertical, _) -> (-.sx, sy)
        in
        {ball with date; position; speed}
    end

    type t =
      | Wall of {
        ball_index: int;
        wall: Wall.t;
      }

    let to_public ~date ~balls_before ~balls_after = function
      | Wall {ball_index; wall} ->
        assert (ball_index = 0);
        let wall = Wall.to_public wall
        and before = Ball.to_public ~date balls_before.(ball_index)
        and after = Ball.to_public ~date balls_after.(ball_index) in
        Public.Event.BallWallCollision {wall; before; after}

    let apply ~date balls = function
      | Wall {ball_index; wall} ->
        assert (ball_index = 0);
        let balls = OCSA.copy balls in
        balls.(ball_index) <- Wall.apply ~date ~ball:balls.(ball_index) wall;
        balls
  end

  module EventQueue = struct
    module PQ = PriQu.Make(struct
      type t = float
      let compare x y =
        Fl.compare y x
    end)

    type t = Event.t PQ.t

    let empty = PQ.empty

    let add events ~date ~event =
      PQ.add events ~k:date ~v:event

    let next events =
      PQ.max events

    let pop_next events =
      PQ.pop_max events
  end

  type t = {
    dimensions: float * float;
    date: float;
    (* Should we use a SortedMap instead of an array?
    At each event, we're copying the entire array to modify just a few balls.
    With a map we would share large parts of the tree between successive states. *)
    balls: Ball.t array;
    events: EventQueue.t
  }

  let create ?(date=0.) ~dimensions balls =
    assert (Li.size balls = 1); (* We're ignoring ball-ball collisions for now, so we make sure we have just one ball. *)
    let balls = Li.map ~f:(Ball.of_public ~date) balls in
    let events =
      balls
      |> Li.fold_i ~init:EventQueue.empty ~f:(fun ~i events ball ->
        Event.Wall.[Vertical; Horizontal]
        |> Li.fold ~init:events ~f:(fun events direction ->
          Event.Wall.next ~direction ~dimensions ball
          |> Opt.value_map ~def:events ~f:(fun (date, wall) ->
            assert (i = 0);
            EventQueue.add events ~date ~event:(Event.Wall {ball_index=i; wall})
          )
        )
      )
    and balls =
      Li.to_array balls
    in
    {dimensions; date; balls; events}

  let dimensions {dimensions; _} =
    dimensions

  let date {date; _} =
    date

  let balls {date; balls; _} =
    balls
    |> Li.of_array
    |> Li.map ~f:(Ball.to_public ~date)

  let advance ({events; balls=balls_before; _} as simulation) ~max_date =
    let (date, event) = EventQueue.next events in
    if date < max_date then
      let balls_after = Event.apply ~date balls_before event in
      let event = Event.to_public ~date ~balls_before ~balls_after event
      and events = EventQueue.pop_next events in
      (Some event, {simulation with date; balls=balls_after; events})
    else
      (None, {simulation with date=max_date})
end

include Public

type t = Internal.t

let create = Internal.create
let dimensions = Internal.dimensions
let date = Internal.date
let balls = Internal.balls
let advance = Internal.advance
