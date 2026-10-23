type sampling = External | Outcome of { epsilon : float }

val outcome : sampling
val sampling_name : sampling -> string

module Make (G : Game.S) : sig
  type t

  val create :
    ?seed:int -> ?weighting:Cfr.algorithm -> ?interval:int -> sampling -> t

  val traverse : t -> Game.player -> unit
  val iterate : t -> unit
  val run : t -> int -> unit
  val split : t -> t
  val iterations : t -> int
  val touched : t -> int
  val average_strategy : t -> Strategy.t
  val current_strategy : t -> Strategy.t
  val regrets : t -> (string * float array) list
  val strategy_sums : t -> (string * float array) list
end
