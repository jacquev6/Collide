open General.Abbr

module Json = struct
  type t = Yojson.Basic.json

  module Of = struct
    let float (v: float) =
      `Float v

    let pair (f:'a -> t) ((a:'a), (b:'a)) =
      `List [f a; f b]

    let assoc (members: (string * t) list) =
      `Assoc members

    let list (f:'a -> t) (xs: 'a list) =
      `List (Li.map ~f xs)
  end

  module To = struct
    open Yojson.Basic.Util

    let fail () =
      Exn.failure "Error while decoding JSON"

    let float = to_float

    let member = member

    let list f xs =
      xs
      |> to_list
      |> Li.map ~f

    let pair f = function
      | `List [a; b] -> (f a, f b)
      | _ -> fail ()
  end
end

let to_json simulation = Json.Of.(
  let (width, height) = Simulation.dimensions simulation
  and balls =
    simulation
    |> Simulation.balls
    |> list (fun {Simulation.Ball.radius; density; position; velocity} ->
      assoc [
        ("radius", float radius);
        ("density", float density);
        ("position", pair float position);
        ("velocity", pair float velocity);
      ]
    )
  in
  assoc [
    ("width", float width);
    ("height", float height);
    ("balls", balls);
  ]
)

let of_json j = Json.To.(
  let width = j |> member "width" |> float
  and height = j |> member "height" |> float
  and balls =
    j
    |> member "balls"
    |> list (fun j ->
      let radius = j |> member "radius" |> float
      and density = j |> member "density" |> float
      and position = j |> member "position" |> pair float
      and velocity = j |> member "velocity" |> pair float in
      {Simulation.Ball.radius; density; position; velocity}
    )
  in
  Simulation.create ~dimensions:(width, height) balls
)
