let games : (string * (module Game.S)) list =
  [
    ("kuhn", (module Kuhn));
    ("leduc", (module Leduc));
    ("liars", Liars_dice.game Liars_dice.standard);
    ("liars-nowild", Liars_dice.game { Liars_dice.standard with wild = false });
    ("liars-1d4", Liars_dice.game { dice = 1; faces = 4; wild = true });
    ("liars-2d3", Liars_dice.game { dice = 2; faces = 3; wild = true });
    ("goofspiel", Goofspiel.game { Goofspiel.standard with cards = 5 });
    ("goofspiel-4", Goofspiel.game { Goofspiel.standard with cards = 4 });
    ( "goofspiel-random",
      Goofspiel.game { Goofspiel.standard with order = Random } );
    ( "goofspiel-open",
      Goofspiel.game
        { Goofspiel.standard with cards = 5; payoff = Points; reveal = true } );
  ]

let algorithms =
  [
    ("cfr", Cfr.Vanilla);
    ("cfr+", Cfr.Plus);
    ("lcfr", Cfr.Linear);
    ("dcfr", Cfr.dcfr);
    ("dcfr+", Cfr.dcfr_plus);
    ("pcfr+", Cfr.pcfr_plus);
    ("pdcfr+", Cfr.pdcfr_plus);
  ]

let samplings =
  [
    ("full", None);
    ("external", Some Mccfr.External);
    ("outcome", Some Mccfr.outcome);
  ]

let game name = List.assoc_opt name games
let algorithm name = List.assoc_opt name algorithms
let sampling name = List.assoc_opt name samplings
