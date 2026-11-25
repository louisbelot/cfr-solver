val make :
  ?label:(int -> Hand.card list -> Hand.card * Hand.card -> int option) ->
  Bet_tree.config ->
  board:Hand.card array ->
  Range.t ->
  Range.t ->
  (module Game.S)
