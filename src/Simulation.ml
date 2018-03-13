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

    (*BISECT-IGNORE-BEGIN*) (* Test code *)
    let repr {radius; density; position=(x, y); velocity=(vx, vy)} =
      Frmt.apply
        "{radius=%.2f; density=%.2f; position=(%.2f, %.2f); velocity=(%.2f, %.2f)}"
        radius density x y vx vy
    (*BISECT-IGNORE-END*)
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

    (*BISECT-IGNORE-BEGIN*) (* Test code *)
    let repr = function
      | BallBallCollision {before=(b1, b2); after=(a1, a2)} ->
        Frmt.apply "BallBallCollision {before=(%s, %s); after=(%s, %s)}" (Ball.repr b1) (Ball.repr b2) (Ball.repr a1) (Ball.repr a2)
      | WallBallCollision {wall; before; after} ->
        Frmt.apply "WallBallCollision {wall=%s; before=%s; after=%s}" (Wall.repr wall) (Ball.repr before) (Ball.repr after)
    (*BISECT-IGNORE-END*)
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

    module BallBall = struct
      let next {Ball.radius=r1; density=_; date=t1; position=(x1, y1); velocity=(vx1, vy1)} {Ball.radius=r2; density=_; date=t2; position=(x2, y2); velocity=(vx2, vy2)} =
        (* Collision at t (to be solved for t) *)
        (* <=> |position ball_1 - position ball_2| = r1 +. r2 *)
        (* <=> |position ball_1 - position ball_2|² = (r1 +. r2)² *)
        let rr = (r1 +. r2) ** 2.
        (* <=> |position ball_1 - position ball_2|² = rr *)
        (* <=> |(p1 + (t - t1) * v1) - (p2 + (t - t2) *v)|² = rr *)
        (* <=> |p1 + t * v1 - t1 * v1 - p2 - t * v2 + t2 * v2|² = rr *)
        (* <=> |p1 - p2 - t1 * v1 + t2 * v2 + (v1 - v2) * t|² = rr *)
        (* <=> (x1 - x2 - t1 * vx1 + t2 * vx2 + (vx1 - vx2) * t)² + (y1 - y2 - t1 * vy1 + t2 * vy2 + (vy1 - vy2) * t)² = rr *)
        and dx = x1 -. x2 -. t1 *. vx1 +. t2 *. vx2
        and dy = y1 -. y2 -. t1 *. vy1 +. t2 *. vy2
        and dvx = vx1 -. vx2
        and dvy = vy1 -. vy2 in
        (* <=> (dx + dvx * t)² + (dy + dvy * t)² = rr *)
        (* <=> (dvx² + dvy²) * t² + 2 * (dx * dvx + dy * dvy) * t + (dx² + dy² - rr) == 0 *)
        let a = dvx ** 2. +. dvy ** 2.
        and b = dx *. dvx +. dy *. dvy
        and c = dx ** 2. +. dy ** 2. -. rr in
        (* <=> a * t² + 2 * b * t + c == 0 *)
        (* Some properties of this parabola:
            - it's concave (decreasing first then increasing) because two constant-velocity objects first
              get closer to each other, then get further.
              Also, this is proven by the fact that a is a sum of squares, hence positive or null. *)
        assert (a >= 0.);
        (*  - it degenerates to an horizontal line when v1 == v2, meaning that two objects with same velocity
              have a constant distance between them.
              This is consistent with a being the norm of v1 - v2, null in that case.
              And if a == 0, then dvx == 0 and dvy == 0, so b == 0 too: two objects with same velocity
              never collide (unless they always touch each other, which we don't model) *)
        let delta = b ** 2. -. a *. c in
        Opt.some_if' (a <> 0. && delta >= 0.) ((-.b -. Fl.sqrt(delta)) /. a)

      let apply ~date ({Ball.radius=r1; density=d1; velocity=(vx1, vy1); _} as ball_1) ({Ball.radius=r2; density=d2; velocity=(vx2, vy2); _} as ball_2) =
        (* "All models are wrong, some are useful" http://en.wikiquote.org/wiki/George_E._P._Box#Empirical_Model-Building_and_Response_Surfaces_.281987.29
        So we use the model of a perfect elastic collision, neglecting energy dissipation, spin, etc.
        - total energy is unchanged: m1 * |v1|² + m2 * |v2|² = const
        - total movement is unchanged: m1 * v1 + m2 * v2 = const
        - force impulse is aligned with the centers, so there is no change on the components of velocity normal to this vector *)
        let (x1, y1) = Ball.position ~date ball_1
        and (x2, y2) = Ball.position ~date ball_2 in
        (* Normal vector *)
        let length = Fl.sqrt ((x2 -. x1) ** 2. +. (y2 -. y1) ** 2.) in
        let nx = (x2 -. x1) /. length
        and ny = (y2 -. y1) /. length in
        (* Relative velocity *)
        let v = (vx2 -. vx1) *. nx +. (vy2 -. vy1) *. ny in
        let vx = v *. nx
        and vy = v *. ny in
        (* Masses (divided by Fl.pi) *)
        let m1 = r1 ** 2. *. d1
        and m2 = r2 ** 2. *. d2 in
        (* Resulting velocities *)
        let vx1 = vx1 +. 2. *. m2 /. (m1 +. m2) *. vx
        and vy1 = vy1 +. 2. *. m2 /. (m1 +. m2) *. vy
        and vx2 = vx2 -. 2. *. m1 /. (m1 +. m2) *. vx
        and vy2 = vy2 -. 2. *. m1 /. (m1 +. m2) *. vy in
        (
          {ball_1 with date; position=(x1, y1); velocity=(vx1, vy1)},
          {ball_2 with date; position=(x2, y2); velocity=(vx2, vy2)}
        )
    end

    type t =
      | WallBall of Wall.t * int
      | BallBall of int * int

    let repr = function
      | WallBall (wall, ball_index) ->
        Frmt.apply "WallBall (%s, %i)" (Wall.repr wall) ball_index
      | BallBall (ball_index_1, ball_index_2) ->
        Frmt.apply "BallBall (%i, %i)" ball_index_1 ball_index_2

    let to_public ~date ~balls_before ~balls_after = function
      | WallBall (wall, ball_index) ->
        let before = Ball.to_public ~date balls_before.(ball_index)
        and after = Ball.to_public ~date balls_after.(ball_index) in
        Public.Event.WallBallCollision {wall; before; after}
      | BallBall (ball_index_1, ball_index_2) ->
        let before = (Ball.to_public ~date balls_before.(ball_index_1), Ball.to_public ~date balls_before.(ball_index_2))
        and after = (Ball.to_public ~date balls_after.(ball_index_1), Ball.to_public ~date balls_after.(ball_index_2)) in
        Public.Event.BallBallCollision {before; after}

    let apply ~date balls = function
      | WallBall (wall, ball_index) ->
        let balls = OCSA.copy balls in
        balls.(ball_index) <- WallBall.apply ~date wall balls.(ball_index);
        (balls, [ball_index])
      | BallBall (ball_index_1, ball_index_2) ->
        let balls = OCSA.copy balls in
        let (ball_1, ball_2) = BallBall.apply ~date balls.(ball_index_1) balls.(ball_index_2) in
        balls.(ball_index_1) <- ball_1;
        balls.(ball_index_2) <- ball_2;
        (balls, [ball_index_1; ball_index_2])
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

  let schedule_collisions ~start ~dimensions ~date ~collisions_at_date ~events ~balls ball_index =
    let events =
      Wall.all
      |> Li.filter_map ~f:(fun wall ->
        Collision.WallBall.next ~dimensions wall balls.(ball_index)
        |> Opt.map ~f:(fun happens_at ->
          let collision = Collision.WallBall (wall, ball_index) in
          {Event.scheduled_at=date; happens_at; collision}
        )
      )
      |> schedule_events ~events ~date ~collisions_at_date
    in
    IntRa.make ~start (Ar.size balls)
    |> IntRa.ToList.filter_map ~f:(fun ball_index_2 ->
      Collision.BallBall.next balls.(ball_index) balls.(ball_index_2)
      |> Opt.map ~f:(fun happens_at ->
          let collision = Collision.BallBall (ball_index, ball_index_2) in
          {Event.scheduled_at=date; happens_at; collision}
      )
    )
    |> schedule_events ~events ~date ~collisions_at_date

  let create ~dimensions balls =
    log "Creating\n";
    let date = 0.
    and collisions_at_date = SoSet.Poly.empty in
    let balls_list = Li.map ~f:(Ball.of_public ~date) balls in
    let balls = Li.to_array balls_list in
    let events =
      IntRa.make (Ar.size balls)
      |> IntRa.fold ~init:EventQueue.empty ~f:(fun events ball_index ->
        schedule_collisions ~start:ball_index ~dimensions ~date ~collisions_at_date ~events ~balls ball_index
      )
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
      let skip =
        match collision with
          | Collision.WallBall (_, ball_index) ->
            balls.(ball_index).Ball.date > scheduled_at
          | Collision.BallBall (ball_index_1, ball_index_2) ->
            balls.(ball_index_1).Ball.date > scheduled_at
            || balls.(ball_index_2).Ball.date > scheduled_at
      in
      if skip then begin
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
          schedule_collisions ~start:0 ~collisions_at_date ~dimensions ~date ~events ~balls:balls_after ball_index
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
