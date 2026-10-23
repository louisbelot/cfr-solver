type mode = Shared | Private

val mode_name : mode -> string

module Make (G : Game.S) : sig
  type t

  val create :
    ?seed:int ->
    ?weighting:Cfr.algorithm ->
    ?batch:int ->
    ?mode:mode ->
    ?pool:Pool.t ->
    domains:int ->
    Mccfr.sampling ->
    t

  val run : t -> int -> unit
  val domains : t -> int
  val rounds : t -> int
  val iterations : t -> int
  val touched : t -> int
  val average_strategy : t -> Strategy.t
  val current_strategy : t -> Strategy.t
  val regrets : t -> (string * float array) list
  val strategy_sums : t -> (string * float array) list
end
