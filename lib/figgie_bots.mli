val passive : Figgie.bot
val noise : Figgie.bot
val fundamentalist : ?edge:float -> unit -> Figgie.bot
val counter : ?edge:float -> unit -> Figgie.bot
val bottom_feeder : Figgie.bot
val all : Figgie.bot list
val find : string -> Figgie.bot option

type stats = { mean : float; stderr : float }

val tournament :
  Rng.t -> games:int -> ?ticks:int -> Figgie.bot array -> stats array
