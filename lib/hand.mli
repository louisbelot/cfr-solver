type card = int

val rank : card -> int
val suit : card -> int
val card : int -> int -> card
val of_string : string -> card
val to_string : card -> string
val parse : string -> card array

type category =
  | High_card
  | Pair
  | Two_pair
  | Trips
  | Straight
  | Flush
  | Full_house
  | Quads
  | Straight_flush

val eval : card array -> int
val popcount : int -> int
val category : int -> category
val category_name : category -> string
