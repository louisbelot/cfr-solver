val probs : Multi_tree.t -> Strategy.t -> float array
val values : Multi_tree.t -> Strategy.t -> float array
val payoff : Multi_tree.t -> float array -> float array
val best : Multi_tree.t -> int -> Strategy.t -> float * Strategy.t
val gains : Multi_tree.t -> Strategy.t -> float array
val nash_conv : Multi_tree.t -> Strategy.t -> float

module Make (G : Multi.S) : sig
  val value : int -> Strategy.t -> float
  val strategy : int -> Strategy.t -> Strategy.t
  val gains : Strategy.t -> float array
  val nash_conv : Strategy.t -> float
end

val strategy : Multi_tree.t -> int -> float array -> Strategy.t
