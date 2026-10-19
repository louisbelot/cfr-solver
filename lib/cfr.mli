(** Vanilla counterfactual regret minimization (Zinkevich et al., 2007).

    Each iteration walks the full tree once and updates both players
    simultaneously. Regrets at an infoset I are weighted by the counterfactual
    reach of each history (chance times the opponent), and the cumulative
    strategy by the player's own reach. In a two-player zero-sum game the
    average strategy converges to a Nash equilibrium, with exploitability
    O(1/sqrt T) in the worst case. *)

module Make (G : Game.S) : sig
  type t

  val create : unit -> t
  val iterate : t -> unit
  val run : t -> int -> unit
  val iterations : t -> int

  val average_strategy : t -> Strategy.t
  (** The strategy that converges. *)

  val current_strategy : t -> Strategy.t
  (** What regret matching would play next; this one need not converge. *)
end
