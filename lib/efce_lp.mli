type objective = Welfare | Player of int
type plan
type result = { values : float array; vars : int; rows : int; x : float array }

val plan : Multi_tree.t -> plan
val pairs : plan -> int
val treeplex : plan -> Treeplex.t
val leaves : plan -> (int * int * float array) list
val index : plan -> int -> int -> int option
val consistency : plan -> (int * int array) list
val residual : plan -> float array -> float

val solve :
  ?exact:bool -> ?objective:objective -> ?slack:float -> plan -> result option

val mass : plan -> result -> int -> int -> float
