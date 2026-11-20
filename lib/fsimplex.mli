type problem = {
  m : int;
  first_art : int;
  columns : (int * float) array array;
  b : float array;
  cost : float array;
  start : int array;
}

type status = Optimal | Infeasible | Unbounded | Stopped

type result = {
  status : status;
  basis : int array;
  value : float;
  x : float array;
  pivots : int;
  refactors : int;
}

val solve : ?limit:int -> problem -> result
