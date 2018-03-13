module Wall: sig
  type t =
    | Left
    | Right
    | Top
    | Bottom

  val repr: t -> string
end

module Ball: sig
  type t = {
    radius: float;
    density: float;
    position: float * float;
    velocity: float * float;
  }

  val repr: t -> string
end

type t

val create: dimensions:float * float -> Ball.t list -> t

module Event: sig
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

  val repr: t -> string
end

val dimensions: t -> float * float

val date: t -> float

val balls: t -> Ball.t list

val advance: t -> max_date:float -> Event.t option * t
