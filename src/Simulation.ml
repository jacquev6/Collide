open General.Abbr
module OCSA = OCamlStandard.ArrayLabels

module Public = struct
  module Wall = struct
    type t =
      | Left
      | Right
      | Top
      | Bottom

    let repr = function
      | Left -> "Left"
      | Right -> "Right"
      | Top -> "Top"
      | Bottom -> "Bottom"
  end

  module Ball = struct
    type t = {
      radius: float;
      density: float;
      position: float * float;
      velocity: float * float;
    }

    let repr {radius; density; position=(x, y); velocity=(vx, vy)} =
      Frmt.apply
        "{radius=%.2f; density=%.2f; position=(%.2f, %.2f); velocity=(%.2f, %.2f)}"
        radius density x y vx vy
  end

  module Event = struct
    type t =
      | BallBallCollision of {
        before: Ball.t * Ball.t;
        after: Ball.t * Ball.t;
      }
      | WallBallCollision of {
        wall: Wall.t;
        before: Ball.t;
        after: Ball.t;
      }

    let repr = function
      | BallBallCollision _ ->
        Exn.failure "@todo Implement"
      | WallBallCollision {wall; before; after} ->
        Frmt.apply "WallBallCollision {wall=%s; before=%s; after=%s}" (Wall.repr wall) (Ball.repr before) (Ball.repr after)
  end
end

module Internal = struct
  let log format =
    StdOut.print ~flush:true format

  module Wall = struct
    include Public.Wall

    let all = [Left; Right; Top; Bottom]
  end

  module Ball = struct
    type t = {
      radius: float;
      density: float;
      date: float;
      position: float * float;
      velocity: float * float;
    }

    (* let repr {radius; density; date; position=(x, y); velocity=(vx, vy)} =
      Frmt.apply
        "{radius=%.2f; density=%.2f; date=%.2f; position=(%.2f, %.2f); velocity=(%.2f, %.2f)}"
        radius density date x y vx vy *)

    let of_public ~date {Public.Ball.radius; density; position; velocity} =
      {radius; density; date; position; velocity}

    let position ~date {date=t0; position=(x, y); velocity=(vx, vy); _} =
      assert (date >= t0);
      let dt = date -. t0 in
      let x = x +. dt *. vx
      and y = y +. dt *. vy in
      (x, y)

    let to_public ~date ({radius; density; velocity; _} as ball) =
      let position = position ~date ball in
      {Public.Ball.radius; density; position; velocity}
  end

  module Collision = struct
    module WallBall = struct
      let next ~dimensions:(w, h) (wall: Wall.t) {Ball.radius; density=_; date; position=(x, y); velocity=(vx, vy)} =
        let (dimension, position, velocity) =
          match wall with
            | Left | Right -> (w, x, vx)
            | Top | Bottom -> (h, y, vy)
        in
        let (velocity_sign, target_position) =
          match wall with
            | Left | Top -> (-1., 0. +. radius)
            | Right | Bottom -> (1., dimension -. radius)
        in
        Opt.some_if' (velocity *. velocity_sign > 0.) (date +. (target_position -. position) /. velocity)

      let apply ~date (wall: Wall.t) ball =
        let position = Ball.position ~date ball
        and velocity =
          let (vx, vy) = ball.Ball.velocity in
          match wall with
            | Left -> assert (vx < 0.); (-.vx, vy)
            | Right -> assert (vx > 0.); (-.vx, vy)
            | Top -> assert (vy < 0.); (vx, -.vy)
            | Bottom -> assert (vy > 0.); (vx, -.vy)
        in
        {ball with date; position; velocity}
    end

    type t =
      | WallBall of Wall.t * int

    let repr = function
      | WallBall (wall, ball_index) ->
        Frmt.apply "WallBall (%s, %i)" (Wall.repr wall) ball_index

    let to_public ~date ~balls_before ~balls_after = function
      | WallBall (wall, ball_index) ->
        let before = Ball.to_public ~date balls_before.(ball_index)
        and after = Ball.to_public ~date balls_after.(ball_index) in
        Public.Event.WallBallCollision {wall; before; after}

    let apply ~date balls = function
      | WallBall (wall, ball_index) ->
        let balls = OCSA.copy balls in
        balls.(ball_index) <- WallBall.apply ~date wall balls.(ball_index);
        (balls, [ball_index])
  end

  module Event = struct
    type t = {
      scheduled_at: float;
      happens_at: float;
      collision: Collision.t;
    }

    let repr {scheduled_at; happens_at; collision} =
      Frmt.apply "{scheduled_at=%.2f; happens_at=%.2f; collision=%s}" scheduled_at happens_at (Collision.repr collision)
  end

  module EventQueue = struct
    module PQ = PriQu.Make(struct
      type t = float
      let compare x y =
        Fl.compare y x
    end)

    type t = Event.t PQ.t

    let empty = PQ.empty

    let add events ~event =
      let k = event.Event.happens_at in
      PQ.add events ~k ~v:event

    let next events =
      events
      |> PQ.max
      |> Tu2.get_1

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
    events: EventQueue.t;
    collisions_at_date: Collision.t SoSet.Poly.t;
  }

  let schedule_events ~events ~date ~collisions_at_date new_events =
    new_events
    |> Li.fold ~init:events ~f:(fun events ({Event.happens_at; collision; _} as event) ->
      if happens_at < date || SoSet.Poly.contains collisions_at_date ~v:collision
      then
        events
      else begin
        log "Scheduling %s\n" (Event.repr event);
        EventQueue.add events ~event
      end
    )

  let schedule_wall_ball_collisions ~dimensions ~date ~collisions_at_date ~events ball_index ball =
    Wall.all
    |> Li.filter_map ~f:(fun wall ->
      Collision.WallBall.next ~dimensions wall ball
      |> Opt.map ~f:(fun happens_at ->
        let collision = Collision.WallBall (wall, ball_index) in
        {Event.scheduled_at=date; happens_at; collision}
      )
    )
    |> schedule_events ~events ~date ~collisions_at_date

  let create ~dimensions balls =
    log "Creating\n";
    assert (Li.size balls = 1); (* We're ignoring ball-ball collisions for now, so we make sure we have just one ball. *)
    let date = 0.
    and collisions_at_date = SoSet.Poly.empty in
    let balls = Li.map ~f:(Ball.of_public ~date) balls in
    let events =
      balls
      |> Li.fold_i ~init:EventQueue.empty ~f:(fun ~i events ball ->
        schedule_wall_ball_collisions ~dimensions ~date ~collisions_at_date ~events i ball
      )
    and balls =
      Li.to_array balls
    in
    {dimensions; date; balls; events; collisions_at_date}

  let dimensions {dimensions; _} =
    dimensions

  let date {date; _} =
    date

  let balls {date; balls; _} =
    balls
    |> Li.of_array
    |> Li.map ~f:(Ball.to_public ~date)

  let skip_canceled_events ~balls events =
    let rec aux events =
      let ({Event.scheduled_at; collision; _} as event) = EventQueue.next events in
      match collision with
        | Collision.WallBall (_, ball_index) ->
          if balls.(ball_index).Ball.date > scheduled_at then begin
            log "Skipping %s\n" (Event.repr event);
            aux (EventQueue.pop_next events)
          end else
            events
    in
    aux events

  let advance ({dimensions; date; events; balls; collisions_at_date} as simulation) ~max_date =
    log "Advancing to %.2f\n" max_date;
    let events = skip_canceled_events ~balls events in
    let ({Event.happens_at; collision; _} as event) = EventQueue.next events in
    if happens_at < max_date then begin
      let collisions_at_date =
        if happens_at > date then
          SoSet.Poly.of_list [collision]
        else
          SoSet.Poly.replace collisions_at_date ~v:collision
      in
      log "Executing %s\n" (Event.repr event);
      let date = happens_at in
      let (balls_after, impacted_ball_indexes) = Collision.apply ~date balls collision in
      let event = Collision.to_public ~date ~balls_before:balls ~balls_after collision
      and events = EventQueue.pop_next events in
      let events =
        impacted_ball_indexes
        |> Li.fold ~init:events ~f:(fun events ball_index ->
          schedule_wall_ball_collisions ~collisions_at_date ~dimensions ~date ~events ball_index balls_after.(ball_index)
        )
      in
      (Some event, {simulation with date; balls=balls_after; events; collisions_at_date})
    end else
      (None, {simulation with date=max_date; events})
end

include Public

type t = Internal.t

let create = Internal.create
let dimensions = Internal.dimensions
let date = Internal.date
let balls = Internal.balls
let advance = Internal.advance
