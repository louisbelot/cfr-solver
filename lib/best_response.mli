(** Exact best responses and exploitability.

    For a profile sigma, the best response of player i is the strategy that
    maximizes i's expected payoff while the opponent keeps playing sigma. It is
    computed bottom-up: at each of i's infosets, pick the action with the
    highest value summed over the infoset's histories, each weighted by the
    probability that chance and the opponent reach it.

    NashConv is the sum of both players' best-response values. In a two-player
    zero-sum game the game values cancel, so NashConv is zero exactly at a Nash
    equilibrium, and exploitability is half of it (OpenSpiel's convention). *)

module Make (G : Game.S) : sig
  val value : Game.player -> Strategy.t -> float
  (** What the player earns by best-responding to the other player's half of the
      profile. *)

  val strategy : Game.player -> Strategy.t -> Strategy.t
  (** A pure best response, with an entry for every infoset of the player. Ties,
      and infosets the opponent never lets the game reach, go to the lowest
      action index. *)

  val nash_conv : Strategy.t -> float
  val exploitability : Strategy.t -> float
end
