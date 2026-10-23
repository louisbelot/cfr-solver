type algorithm =
  | Vanilla
  | Plus
  | Linear
  | Discounted of { alpha : float; beta : float; gamma : float }

val dcfr : algorithm
val algorithm_name : algorithm -> string
val discounts : algorithm -> int -> float * float * float

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
