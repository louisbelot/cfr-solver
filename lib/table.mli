type t = {
  infosets : Tree.infoset array;
  player : Game.player array;
  offset : int array;
  regret : float array;
  strategy_sum : float array;
  current : float array;
  stamp : int array;
}

val create : Tree.t -> t
val copy : t -> t
val share : t -> t
val slots : t -> int
val discount : t -> float * float * float -> unit
val discount_range : t -> int -> int -> float * float * float -> unit
val blit : src:t -> dst:t -> unit
val refresh : t -> int -> int -> unit
val average_strategy : t -> Strategy.t
val current_strategy : t -> Strategy.t
val regrets : t -> (string * float array) list
val strategy_sums : t -> (string * float array) list
