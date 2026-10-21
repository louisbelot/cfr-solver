(** Counterfactual regret minimization (Zinkevich et al., 2007) and its weighted
    descendants.

    Each iteration walks the full tree. Regrets at an infoset I are weighted by
    the counterfactual reach of each history (chance times the opponent), and
    the cumulative strategy by the player's own reach. In a two-player zero-sum
    game the average strategy converges to a Nash equilibrium.

    The variants differ in how they weight iterations. Discounted CFR (Brown and
    Sandholm, 2019) covers all of them: at the end of iteration t, accumulated
    positive regrets are multiplied by t^alpha / (t^alpha + 1), negative ones by
    t^beta / (t^beta + 1), and iteration t enters the average strategy with
    weight t^gamma. An exponent of [infinity] leaves regrets of that sign
    untouched and [neg_infinity] resets them to zero. In those terms:

    - vanilla CFR is (infinity, infinity, 0), with simultaneous updates;
    - CFR+ (Tammelin, 2014) is (infinity, neg_infinity, 1): regret matching+
      with linear averaging;
    - Linear CFR is (1, 1, 1): iteration t weighted by t everywhere.

    Every variant except vanilla updates the players alternately: player 0
    first, then player 1 against player 0's new strategy. *)

type algorithm =
  | Vanilla
  | Plus
  | Linear
  | Discounted of { alpha : float; beta : float; gamma : float }

val dcfr : algorithm
(** [Discounted { alpha = 1.5; beta = 0.; gamma = 2. }], the setting Brown and
    Sandholm recommend. *)

val algorithm_name : algorithm -> string
(** ["cfr"], ["cfr+"], ["lcfr"], or ["dcfr"] followed by the three parameters.
*)

module Make (G : Game.S) : sig
  type t

  val create : ?algorithm:algorithm -> unit -> t
  (** Vanilla CFR by default. *)

  val iterate : t -> unit
  val run : t -> int -> unit
  val iterations : t -> int

  val average_strategy : t -> Strategy.t
  (** The strategy that converges. *)

  val current_strategy : t -> Strategy.t
  (** What regret matching would play next; this one need not converge. *)
end
