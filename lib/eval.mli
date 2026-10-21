module Make (G : Game.S) : sig
  val expected_value : Strategy.t -> float
  val infosets : Game.player -> (string * string array) list
end
