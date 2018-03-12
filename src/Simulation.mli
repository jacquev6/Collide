module Wall: sig
  type t =
    | Left
    | Right
    | Top
    | Bottom
end

module Ball: sig
  type t = {
    radius: float;
    density: float;
    position: float * float;
    speed: float * float;
  }
end

type t

val create: ?date:float -> dimensions:float * float -> Ball.t list -> t

module Event: sig
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

val dimensions: t -> float * float

val date: t -> float

val balls: t -> Ball.t list

val advance: t -> max_date:float -> Event.t option * t
