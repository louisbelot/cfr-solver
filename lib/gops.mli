module type FIELD = sig
  include Lp.FIELD

  val matrix : t array array -> t array * t array * t
end

module type S = sig
  type field
  type t

  val create : Goofspiel.order -> t
  val size : t -> int
  val value : t -> int -> int -> int -> field
  val turn : t -> int -> int -> int -> int -> field array * field array * field
end

module Make (F : FIELD) : S with type field = F.t
module Exact : S with type field = Rat.t
module Float : S with type field = float

val float_matrix : float array array -> float array * float array * float
val strategy : Goofspiel.config -> Strategy.t
