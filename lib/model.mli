type t

val empty : t
val observe : Game.player -> Play.move list -> t -> t
val visits : t -> string -> int
val count : t -> string -> string -> int
val strategy : ?prior:float -> Tree.t -> Game.player -> t -> Strategy.t
