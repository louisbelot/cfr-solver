type t

val build : (module Multi.S) -> t
val sequences : t -> int -> int
val infosets : t -> int -> int
val plan : t -> int -> Strategy.t -> Rat.t array
val behavior : t -> int -> Rat.t array -> (string * Rat.t array) list
val strategy : t -> Rat.t array * Rat.t array -> Strategy.t
val values : t -> Rat.t array * Rat.t array -> Rat.t * Rat.t
val gains : t -> Rat.t array * Rat.t array -> Rat.t * Rat.t

type solution = {
  plans : Rat.t array * Rat.t array;
  values : Rat.t * Rat.t;
  strategy : Strategy.t;
  pivots : int;
}

val solve : ?prior:Strategy.t -> ?max_pivots:int -> t -> solution
