type relation = Le | Eq | Ge
type row = { coeffs : (int * Rat.t) list; rel : relation; rhs : Rat.t }

type result =
  | Optimal of { value : Rat.t; x : Rat.t array; pivots : int }
  | Infeasible
  | Unbounded

val maximize :
  ?bland:bool -> vars:int -> (int * Rat.t) list -> row list -> result
