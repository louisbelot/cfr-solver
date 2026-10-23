type feature = Ehs | Ehs2 | Hist of int
type t = { name : string; labels : int array }

val feature_name : feature -> string
val round1 : ?seed:int -> Leduc.deck -> feature -> int -> t
val round2 : Leduc.deck -> int -> t

val abstraction :
  ?round1:t -> ?round2:t -> ?recall:bool -> Leduc.deck -> Leduc.abstraction
