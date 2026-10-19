(** Exact evaluation by walking the whole game tree. *)

module Make (G : Game.S) : sig
  val expected_value : Strategy.t -> float
  (** Player 0's expected payoff when both players follow the profile. *)

  val infosets : Game.player -> (string * string array) list
  (** The player's infosets and their action labels, in depth-first order of
      first visit. *)
end
