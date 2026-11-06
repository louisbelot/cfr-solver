type t
type profile = float array array array

val create :
  ?algorithm:Cfr.algorithm ->
  ?iso:bool ->
  ?single:bool ->
  ?prune:bool ->
  ?rbp:bool ->
  ?buckets:int list ->
  ?feature:Holdem_buckets.feature ->
  Bet_tree.config ->
  board:Hand.card array ->
  Range.t ->
  Range.t ->
  t

val symmetries : Hand.card list -> Range.t -> Range.t -> Suit.perm list
val iterate : t -> unit
val run : t -> int -> unit
val iterations : t -> int
val bucket : t -> int -> Hand.card list -> Hand.card * Hand.card -> int option
val memory : t -> int
val skipped : t -> int
val pruned : t -> int
val caught_up : t -> int
val decisions : t -> int
val pot : t -> float
val range : t -> int -> Range.t
val average : t -> profile
val current : t -> profile
val regrets : t -> profile
val cbv : t -> int -> profile -> float array

type gadget = Resolve | Maxmargin

val gadget : t -> gadget -> int -> float array -> unit
val entry : t -> float array
val alternatives : t -> float array
val margins : t -> profile -> float array
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
val combine : t -> profile -> profile -> profile

type root = {
  path : string;
  board : Hand.card array;
  pot : float;
  reach : Range.t array;
}

val roots : t -> profile -> root list
val river_config : Bet_tree.config -> root -> Bet_tree.config
val restrict : t -> profile -> string -> t -> profile
val graft : t -> profile -> string -> t -> profile -> int -> profile
val gifts : t -> profile -> int -> (string * float array) list

type solving = Unsafe | Safe of gadget | Reach of gadget

val solving_name : solving -> string

val prepare :
  ?algorithm:Cfr.algorithm ->
  solving ->
  Bet_tree.config ->
  t ->
  profile ->
  root ->
  int ->
  t

val resolve :
  ?algorithm:Cfr.algorithm ->
  solving ->
  iterations:int ->
  Bet_tree.config ->
  t ->
  profile ->
  profile

val translate :
  low:(a:float -> b:float -> float -> float) ->
  abstract:t ->
  profile ->
  t ->
  profile
