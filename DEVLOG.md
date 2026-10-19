# cfr-solver devlog

## 2026-10-19: Kuhn poker solved

First run: the dune scaffold and CI, a game interface, Kuhn
poker, vanilla CFR, exact best response and exploitability, and the CLI with
JSON export.

What exists now:

- `Game.S` (`lib/game.ml`): a history type and `node : history -> history node`,
  where a node is `Terminal`, `Chance` or `Decision` with an infoset key and
  labeled children. Two-player zero-sum only, payoffs stored for player 0.
- `Kuhn`: 55 histories, 30 terminals, 12 infosets. `Kuhn.equilibrium alpha`
  builds the known equilibrium family.
- `Cfr.Make`: vanilla CFR with simultaneous updates.
- `Eval.Make`: expected value of a profile and the list of a player's infosets.
- `Best_response.Make`: exact best response, NashConv and exploitability.
- `Strategy` (immutable string map), `Regret_matching`, and a small `Json`
  writer (no yojson dependency for now).
- `cfr-solver solve [-n N] [--json FILE] [--trace FILE]`, built on cmdliner.

Design notes:

- The library is called `cfr_solver`, not `cfr`. With a module `cfr.ml` inside
  a library named `cfr`, dune treats it as the library's main module and
  hides everything else.
- CFR caches the current strategy per infoset with an iteration stamp. An
  infoset is visited once per history inside it, and regrets change on every
  visit, so without the cache later histories see a strategy built from part
  of the current iteration's regret. Neller and Lanctot's tutorial code works
  that way. It converges just as fast on Kuhn (checked), but the exact
  two-iteration values from OpenSpiel only match with the cache.
- The strategy sum is updated once per history, not once per infoset. With
  perfect recall the player's own reach is the same for every history in an
  infoset, so this scales numerator and denominator by the same constant.
- The best response decides each infoset from all of its histories at once,
  weighted by chance times opponent reach, and memoizes decisions so deeper
  infosets are settled first. Ties go to the lowest action, like OpenSpiel.
  I tried a small relative slack in the argmax in case rounding split exact
  ties, but the one tie in the tests (a king is worth 1.5 either way against
  uniform) comes out exact in floating point, so the plain comparison stayed.
- Infosets are string keys in a `Hashtbl`. Fine for Kuhn and Leduc. Arrays
  can replace them later.
- Windows MinGW printf writes three-digit exponents (`2.318e-003`). Harmless
  in JSON and CSV, so I left it.

Checks:

- 31 tests, about 0.3 s. Game contract walk (chance sums to 1, unique labels,
  one player per infoset, perfect recall), hand-derived payoffs and values,
  -1/18 for the whole equilibrium family.
- OpenSpiel references from `exploitability_test.py` and `cfr_test.py`:
  NashConv 11/12 for uniform, 2 for always-pass, 0 at equilibrium, their
  best-response actions against uniform, and `Qb` = [1/4, 3/4] after two
  iterations.
- qcheck property, 300 random profiles: best-response value equals the max
  over all 64 pure strategies, and the returned strategy earns it.
- JSON floats round-trip (1000 random floats), and Python's `json` module
  parses the CLI output.
- Mutation check: a best response that looks at one history per infoset
  fails 4 tests; CFR without the strategy cache fails 2.

Measured on Kuhn (Windows laptop, native code):

| iterations | exploitability | x sqrt(T) |
|-----------:|---------------:|----------:|
| 100        | 0.0257         | 0.257     |
| 10,000     | 0.00232        | 0.232     |
| 1,000,000  | 0.000219       | 0.219     |

About 275k iterations per second (3.64 s CPU for a million). The
exploitability times sqrt(T) column barely moves, so vanilla CFR here sits on
its worst-case rate.

Next: CFR+ (regret floor at zero, linear averaging, alternating updates) and
the exploitability comparison, then Leduc hold'em, whose uniform NashConv
(4.747222) OpenSpiel also publishes.
