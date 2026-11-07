type history

val game : int -> (module Multi.S with type history = history)
val card_name : players:int -> int -> string
