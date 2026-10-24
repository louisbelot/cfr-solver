type t
type profile = float array array array

val create :
  ?algorithm:Cfr.algorithm ->
  Bet_tree.config ->
  board:Hand.card array ->
  Range.t ->
  Range.t ->
  t

val iterate : t -> unit
val run : t -> int -> unit
val iterations : t -> int
val decisions : t -> int
val pot : t -> float
val range : t -> int -> Range.t
val average : t -> profile
val current : t -> profile
val value : t -> profile -> float
val best_response : t -> int -> profile -> float
val exploitability : t -> profile -> float
val key : Hand.card * Hand.card -> string -> string
val strategy : t -> profile -> Strategy.t

type line = {
  path : string;
  player : int;
  actions : string array;
  freqs : float array;
  mass : float;
}

val lines : ?chance:bool -> t -> profile -> line list
