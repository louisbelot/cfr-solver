type result = {
  lower : Rat.t;
  upper : float;
  root : float;
  nodes : int;
  x : Rat.t array;
  z : Rat.t array;
  trace : (int * float * float) list;
  warm : int;
  pivots : int;
}

val solve : ?gap:float -> ?limit:int -> ?strong:int -> Team.t -> result
