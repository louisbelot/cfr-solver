type t = { hands : (Hand.card * Hand.card) array; weights : float array }

val combos : (Hand.card * Hand.card) array
val index : Hand.card * Hand.card -> int
val combo_to_string : Hand.card * Hand.card -> string
val of_weights : ?board:Hand.card array -> float array -> t
val full : ?board:Hand.card array -> unit -> t
val of_string : ?board:Hand.card array -> string -> t
val size : t -> int
val total : t -> float
val blocks : Hand.card array -> Hand.card * Hand.card -> bool
