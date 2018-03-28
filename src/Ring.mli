type 'a t

val make: int -> 'a t

val add: 'a t -> v:'a -> 'a t

val size: 'a t -> int

val max_size: 'a t -> int

val get: 'a t -> index:int -> 'a

module Test: sig
  val test: General.Testing.Test.t
end
