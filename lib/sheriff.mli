type config = {
  items : int;
  bribe : int;
  rounds : int;
  value : int;
  fine : int;
  compensation : int;
}

val baseline : config
val name : config -> string
val game : config -> (module Multi.S)
