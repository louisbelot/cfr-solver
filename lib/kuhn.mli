(** Kuhn poker (Kuhn, 1950): three cards J < Q < K, one dealt to each player,
    ante 1, one betting round with a bet size of 1.

    Infoset keys are the player's card followed by the moves so far, in the
    style of Neller and Lanctot's tutorial: ["K"], ["Qp"], ["Jpb"]. Every
    decision offers ["p"] then ["b"]. Facing a bet, ["p"] folds and ["b"] calls.
*)

include Game.S

val equilibrium : float -> Strategy.t
(** [equilibrium alpha] is the Nash equilibrium in which player 0 bluffs a jack
    with probability [alpha], bets a king with [3 alpha] and calls with a queen
    with [alpha + 1/3]. Player 1's half is the same for every [alpha]. Every
    member is worth -1/18 to player 0. Raises [Invalid_argument] unless
    [0 <= alpha <= 1/3]. *)
