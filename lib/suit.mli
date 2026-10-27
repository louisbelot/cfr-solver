type perm = int array

val identity : perm
val all : perm array
val apply : perm -> Hand.card -> Hand.card
val compose : perm -> perm -> perm
val inverse : perm -> perm
val combo : perm -> Hand.card * Hand.card -> Hand.card * Hand.card
val fixes : perm -> Hand.card list -> bool
val stabilizer : perm list -> Hand.card list -> perm list

val orbits :
  perm list -> Hand.card list -> (Hand.card * (Hand.card * perm) list) list
