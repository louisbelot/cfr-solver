val games : (string * (module Game.S)) list
val algorithms : (string * Cfr.algorithm) list
val samplings : (string * Mccfr.sampling option) list
val game : string -> (module Game.S) option
val algorithm : string -> Cfr.algorithm option
val sampling : string -> Mccfr.sampling option option
