type t = { name : string; dim : int; encode : string -> float array }

val kuhn : t
val leduc : Leduc.deck -> t
val one_hot : string array -> t
