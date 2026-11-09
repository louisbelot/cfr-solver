type meta =
  | Exact
  | Nash
  | Uniform
  | Prd of { iterations : int; dt : float; gamma : float }
  | Alpharank of { alpha : float; m : int }

type oracle = Best | Cfr of int

type step = {
  iteration : int;
  sizes : int array;
  values : float array;
  nash_conv : float;
  seconds : float;
}

val mixture : Multi_tree.t -> int -> (float * Strategy.t) list -> Strategy.t
val pure : Multi_tree.t -> int -> Strategy.t

val run :
  ?meta:meta ->
  ?oracle:oracle ->
  (module Multi.S) ->
  iterations:int ->
  step list * Strategy.t
