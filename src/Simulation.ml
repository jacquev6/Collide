open General.Abbr

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

type t = {
  dimensions: float * float;
  balls: Ball.t list;
}

let create ?date:_ ~dimensions balls =
  {dimensions; balls}

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

let dimensions {dimensions; _} =
  dimensions

let balls {balls; _} =
  balls

type advance = {
  date: float;
  event: Event.t option;
  simulation: t;
}

let advance _ ~max_date:_ =
  Exn.failure "@todo Implement"
