type objective = Welfare | Player of int
type plan
type result = { values : float array; vars : int; rows : int; x : float array }

val plan : Multi_tree.t -> plan
val pairs : plan -> int
val solve : ?exact:bool -> ?objective:objective -> plan -> result option
val mass : plan -> result -> int -> int -> float
