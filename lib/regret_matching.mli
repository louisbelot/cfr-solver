(** Regret matching (Hart and Mas-Colell, 2000): play each action in proportion
    to its positive cumulative regret, uniformly if none is positive. *)

val strategy : float array -> float array

val strategy_into : float array -> float array -> unit
(** [strategy_into regrets out] writes [strategy regrets] into [out] without
    allocating. *)

val normalize : float array -> float array
(** Scale non-negative weights to sum to 1, or uniform if they are all zero.
    This turns CFR's cumulative strategy into the average strategy. *)

val normalize_into : float array -> float array -> unit
