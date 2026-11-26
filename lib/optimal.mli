type mode = Threshold of float | Penalty of float
type t

val create : ?concept:Efce_lp.concept -> Efce_lp.plan -> t
val range : t -> float * float
val size : t -> int
val triggers : t -> int
val welfare : t -> float array -> float
val values : t -> float array -> float array
val gains : t -> float array -> float array
val gain : t -> float array -> float

type bounds = { lower : float; upper : float; welfare : float; gain : float }
type solver

val solver : ?predictive:bool -> t -> mode -> solver
val iterate : solver -> unit
val run : solver -> int -> unit
val iterations : solver -> int
val average : solver -> float array
val bounds : solver -> bounds

type step = {
  tau : float;
  feasible : bool option;
  iterations : int;
  bounds : bounds;
  xi : float array;
}

val decide :
  ?eps:float ->
  ?every:int ->
  ?limit:int ->
  ?predictive:bool ->
  t ->
  float ->
  step

val bisect :
  ?eps:float ->
  ?every:int ->
  ?limit:int ->
  ?predictive:bool ->
  t ->
  step * step list
