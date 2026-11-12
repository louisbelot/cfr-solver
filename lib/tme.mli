type result = {
  lower : Rat.t;
  upper : float;
  root : float;
  nodes : int;
  x : Rat.t array;
  z : Rat.t array;
  trace : (int * float * float) list;
}

val solve : ?gap:float -> ?limit:int -> Team.t -> result
