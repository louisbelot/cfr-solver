type point = {
  iteration : int;
  touched : int;
  average : float;
  current : float option;
}

type row = {
  iteration : int;
  average : float array;
  current : float array;
  regret : float array;
}

type t

val create :
  ?algorithm:Cfr.algorithm ->
  ?last:bool ->
  ?budget:int ->
  ?growth:float ->
  (module Game.S) ->
  t

val game : t -> (module Game.S)
val infosets : t -> Tree.infoset array
val average : t -> Strategy.t
val iterate : t -> unit
val checkpoint : t -> unit
val iterations : t -> int
val touched : t -> int
val points : t -> point list
val frames : t -> int list
val history : t -> string -> row list option
val table : t -> row array
