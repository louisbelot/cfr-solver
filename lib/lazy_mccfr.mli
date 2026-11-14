type 's node =
  | Terminal of float array
  | Chance of (Rng.t -> 's)
  | Decision of {
      player : int;
      infoset : string;
      actions : string array;
      next : int -> 's;
    }

module type GAME = sig
  type state

  val name : string
  val players : int
  val root : state
  val node : state -> state node
end

module Of_multi (G : Multi.S) : GAME with type state = G.history

type policy = string -> int -> float array

module Make (G : GAME) : sig
  type t

  val create :
    ?seed:int ->
    ?weighting:Cfr.algorithm ->
    ?interval:int ->
    ?fixed:(int -> policy option) ->
    unit ->
    t

  val traverse : t -> int -> unit
  val iterate : t -> unit
  val run : t -> int -> unit
  val iterations : t -> int
  val touched : t -> int
  val infosets : t -> int
  val slots : t -> int
  val average_strategy : t -> Strategy.t
  val current_strategy : t -> Strategy.t
  val average : t -> policy
  val mass : t -> string -> float
  val playout : Rng.t -> (int -> policy) -> float array
end
