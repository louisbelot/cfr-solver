type algorithm =
  | Vanilla
  | Plus
  | Linear
  | Discounted of { alpha : float; beta : float; gamma : float }
  | Predictive of { alpha : float; gamma : float }

val dcfr : algorithm
val dcfr_plus : algorithm
val pcfr_plus : algorithm
val pdcfr_plus : algorithm
val algorithm_name : algorithm -> string
val discounts : algorithm -> int -> float * float * float
val alternating : algorithm -> bool
val predictive : algorithm -> bool
val weight : algorithm -> int -> float

module Make (G : Game.S) : sig
  type t

  val create : ?algorithm:algorithm -> unit -> t
  val iterate : t -> unit
  val run : t -> int -> unit
  val iterations : t -> int
  val touched : t -> int
  val average_strategy : t -> Strategy.t
  val current_strategy : t -> Strategy.t
end
