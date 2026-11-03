type feature = Ehs2 | Hist of int | Emd | Potential of int

val feature_name : feature -> string
val feature_of_string : string -> feature

val labels :
  ?seed:int ->
  ?restarts:int ->
  feature ->
  int ->
  Hand.card array ->
  (Hand.card * Hand.card) array ->
  int array
