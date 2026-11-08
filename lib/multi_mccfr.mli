type averaging = Simple | Weighted | Full

val averaging_name : averaging -> string

module Make (G : Multi.S) : sig
  type t

  val create :
    ?seed:int ->
    ?weighting:Cfr.algorithm ->
    ?interval:int ->
    ?averaging:averaging ->
    unit ->
    t

  val traverse : t -> int -> unit
  val iterate : t -> unit
  val run : t -> int -> unit
  val accumulate : t -> int -> Strategy.t -> unit
  val iterations : t -> int
  val touched : t -> int
  val infosets : t -> int
  val tree : t -> Multi_tree.t
  val average_strategy : t -> Strategy.t
  val current_strategy : t -> Strategy.t
end
