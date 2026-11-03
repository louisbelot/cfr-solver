val hs : Hand.card array -> float array
val ehs2 : Hand.card array -> float array
val rivers : ?quantize:int -> Hand.card array -> float array array
val histogram : bins:int -> Hand.card array -> float array array
