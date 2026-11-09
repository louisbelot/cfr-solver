val uniform : Tree.t -> Game.player -> Strategy.t
val prefer : string list -> Tree.t -> Game.player -> Strategy.t
val part : Tree.t -> Game.player -> Strategy.t -> Strategy.t
val blend : float -> Strategy.t -> Strategy.t -> Strategy.t
