type t
type frozen
type status = Optimal | Bound | Infeasible | Failed

val solve :
  ?limit:int -> vars:int -> (int * Rat.t) list -> Lp.row list -> t * status

val add : ?limit:int -> t -> Lp.row list -> status
val value : t -> float
val x : t -> float array
val violation : t -> float
val rows : t -> int
val pivots : t -> int
val freeze : t -> frozen
val thaw : frozen -> t
val size : frozen -> int
