type learner =
  | Network of { hidden : int list; steps : int; batch : int; lr : float }
  | Exact

type config = {
  traversals : int;
  capacity : int;
  learner : learner;
  average_steps : int;
  domains : int;
  seed : int;
}

val default : config
val learner_name : learner -> string

type t

val create : ?config:config -> Features.t -> Tree.t -> t
val regret_match : float array -> float array
val iterate : t -> unit
val run : t -> int -> unit
val iterations : t -> int
val touched : t -> int
val samples : t -> int
val current : t -> Strategy.t
val average_exact : t -> Strategy.t
val average_network : t -> Strategy.t
