type history

val game : ?ranks:int -> int -> (module Multi.S with type history = history)
val card_name : ranks:int -> int -> string
