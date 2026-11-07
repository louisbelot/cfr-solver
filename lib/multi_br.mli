module Make (G : Multi.S) : sig
  val value : int -> Strategy.t -> float
  val strategy : int -> Strategy.t -> Strategy.t
  val gains : Strategy.t -> float array
  val nash_conv : Strategy.t -> float
end
