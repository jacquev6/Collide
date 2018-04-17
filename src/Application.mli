module Defaults: sig
  val balls: int
  val max_speed: float
  val min_radius: float
  val max_radius: float
  val min_density: float
  val max_density: float
  val velocity_vectors: bool
  val previous_positions: int
end

module Make(C: JsOfOCairo.S): sig
  type t

  val create:
    dimensions:int * int
    -> balls:int -> max_speed:float
    -> min_radius:float -> max_radius:float
    -> min_density:float -> max_density:float
    -> velocity_vectors:bool -> previous_positions:int
    -> t

  val date: t -> float

  val randomize: t
    -> balls:int -> max_speed:float
    -> min_radius:float -> max_radius:float
    -> min_density:float -> max_density:float
    -> unit

  val resize: t -> dimensions:int * int -> unit

  val set_display: t -> velocity_vectors:bool -> previous_positions:int -> unit

  val advance: t -> date:float -> unit

  val draw: t -> context:C.context -> unit
end
