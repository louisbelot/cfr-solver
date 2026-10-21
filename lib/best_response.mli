module Make (G : Game.S) : sig
  val value : Game.player -> Strategy.t -> float
  val strategy : Game.player -> Strategy.t -> Strategy.t
  val nash_conv : Strategy.t -> float
  val exploitability : Strategy.t -> float
end
