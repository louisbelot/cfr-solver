module Make (G : Multi.S) : sig
  type t

  val create : ?algorithm:Cfr.algorithm -> unit -> t
  val iterate : t -> unit
  val run : t -> int -> unit
  val iterations : t -> int
  val touched : t -> int
  val infosets : t -> int
  val tree : t -> Multi_tree.t
  val average_strategy : t -> Strategy.t
  val current_strategy : t -> Strategy.t
end
