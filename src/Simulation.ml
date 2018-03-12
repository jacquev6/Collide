open General.Abbr

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
  module Event = struct
    type dir =
      | Horizontal
      | Vertical

    type loc =
      | Low
      | High

    type t =
      | BallWallCollision of {
        ball: int;
        wall: dir * loc;
      }
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

    (* let pop_next events =
      PQ.pop_max events *)
  end

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

    let to_public ~date {radius; density; date=t0; position=(x, y); speed=(sx, sy)} =
      assert (date >= t0);
      let dt = date -. t0 in
      let x = x +. dt *. sx
      and y = y +. dt *. sy in
      {Public.Ball.radius; density; position=(x, y); speed=(sx, sy)}
  end

  module Collisions = struct
    let wall getter ~dimensions {Ball.radius; density=_; date; position; speed} =
      let dimension = getter dimensions
      and position = getter position
      and speed = getter speed in
      assert (0. +. radius <= position);
      assert (position <= dimension -. radius);
      if speed > 0. then
        Some (Event.High, date +. ((dimension -. radius) -. position) /. speed)
      else if speed < 0. then
        Some (Event.Low, date +. (position -. (0. +. radius)) /. speed)
      else
        None
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
        let events =
          Collisions.wall Tu2.get_0 ~dimensions ball
          |> Opt.value_map ~def:events ~f:(fun (loc, date) ->
            EventQueue.add events ~date ~event:(Event.BallWallCollision {ball=i; wall=(Vertical, loc)})
          )
        in
        let events =
          Collisions.wall Tu2.get_1 ~dimensions ball
          |> Opt.value_map ~def:events ~f:(fun (loc, date) ->
            EventQueue.add events ~date ~event:(Event.BallWallCollision {ball=i; wall=(Horizontal, loc)})
          )
        in
        events
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

  let advance ({events; _} as simulation) ~max_date =
    let (next_date, _) = EventQueue.next events in
    if next_date < max_date then
      Exn.failure "@todo Implement"
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
