type t

val build : (module Game.S) -> t
val sequences : t -> Game.player -> int
val infosets : t -> Game.player -> int
val entries : t -> int
val plan : t -> Game.player -> Strategy.t -> Rat.t array
val strategy : t -> Game.player -> Rat.t array -> Strategy.t
val value : t -> Rat.t array -> Rat.t array -> Rat.t
val best_response : t -> Game.player -> Rat.t array -> Rat.t
val nash_conv : t -> Strategy.t -> Rat.t
val exploitability : t -> Strategy.t -> Rat.t

type solution = {
  value : Rat.t;
  plans : Rat.t array * Rat.t array;
  strategy : Strategy.t;
  pivots : int * int;
}

val solve : ?warm:bool -> t -> solution
