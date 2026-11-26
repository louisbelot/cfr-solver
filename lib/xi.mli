type op =
  | Fill of { src : int; dst : int array; at : int }
  | Sum of { dst : int; src : int array }

type t

val build : Efce_lp.plan -> t
val size : t -> int
val ops : t -> op array
val root : t -> int
val best : t -> float array -> float * float array

type learner

val learner : ?predictive:bool -> t -> learner
val play : learner -> float array
val update : learner -> float array -> unit
