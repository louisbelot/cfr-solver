(** Kuhn poker (Kuhn, 1950): three cards J < Q < K, one dealt to each player,
    ante 1, one betting round with a bet size of 1.

    Infoset keys are the player's card followed by the moves so far, as in
    Neller and Lanctot's tutorial: ["K"], ["Qp"], ["Jpb"]. Every decision offers
    ["p"] then ["b"]. Facing a bet, ["p"] folds and ["b"] calls. *)

include Game.S
