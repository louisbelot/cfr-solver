type t
type plan = { pa : int array; pb : int array }

type solution = {
  value : Rat.t;
  optimal : bool;
  support : (Rat.t * plan) list;
  columns : int;
  trace : (int * float * float) list;
  response : float array;
}

val build : Multi_tree.t -> adversary:int -> t

type plex = { size : int; infos : (int * int * int) array }

val members : t -> int * int
val plexes : t -> plex * plex * plex
val terms : t -> (int * int * int * Rat.t) array
val local_plans : t -> int array * int array
val column : t -> plan -> Rat.t array

val respond :
  ?budget:int -> ?starts:plan list -> t -> float array -> float * plan * bool

val guarantee : t -> (Rat.t * plan) list -> Rat.t
val guard : t -> Rat.t array -> Rat.t array -> Rat.t
val solve : ?budget:int -> ?iterations:int -> t -> solution
val strategies : t -> plan -> Strategy.t
val marginals : t -> (Rat.t * plan) list -> Strategy.t
val realization : t -> Strategy.t -> float array
