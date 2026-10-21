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

## 2026-10-20: CFR+, linear and discounted CFR, Leduc hold'em

CFR+, linear and discounted CFR and Leduc hold'em, plus the compiled tree because
the charts needed the speed, and the first convergence charts.

What exists now:

- `Cfr.algorithm`: `Vanilla | Plus | Linear | Discounted of {alpha; beta; gamma}`,
  with `Cfr.dcfr` for Brown and Sandholm's (1.5, 0, 2). One code path runs all
  of them.
- `Leduc`: OpenSpiel's rules (raises of 2 and 4, two per round, fold only when
  facing a raise, player 0 first in both rounds, ties split). Chance deals ranks,
  not cards, so 288 infosets instead of OpenSpiel's 936, and 1,939 histories.
- `Tree`: the game compiled once into arrays with numbered infosets. `Cfr`
  walks it and keeps all per-action numbers in flat float arrays.
- `cfr-solver solve -g leduc -a dcfr`, and `cfr-solver compare` to run all four
  variants and write a CSV and a log-log SVG (`bin/plot.ml`, no plotting
  dependency).
- `bench/bench.exe`: iterations per second and words allocated per iteration.

Design notes:

- Every variant is a DCFR schedule. At the end of iteration t, positive
  regrets are multiplied by t^a/(t^a+1), negative ones by t^b/(t^b+1), and the
  iteration enters the average with weight t^g. Infinite exponents mean keep
  (a = inf) or reset (b = -inf), so vanilla is (inf, inf, 0), CFR+ is
  (inf, -inf, 1) and linear CFR is (1, 1, 1). Multiplying by t/(t+1) after
  every iteration leaves iteration s weighted s/(T+1) at the end, which is
  exactly linear weighting.
- The average uses the weight t^g directly instead of discounting the sum
  each iteration. Same normalized strategy, no extra pass.
- Discounting happens after the iteration's regret is added. The DCFR paper
  never says so in the main text; Lemma 5's proof applies the factor after the
  update, and that's the reading the reference test encodes.
- CFR+ here means Tammelin's version as OpenSpiel implements it: regret
  matching+, alternating updates, linear averaging, no averaging delay. The
  DCFR paper's experiments weight CFR+'s average by t^2 instead;
  `Discounted {alpha = infinity; beta = neg_infinity; gamma = 2.}` gives that.
- Alternation: player 0's pass, then player 1's pass against player 0's new
  regrets. The strategy cache is stamped per pass, not per iteration, so
  player 1's pass picks up the new strategy. CFR+ with simultaneous updates
  fails OpenSpiel's CFR+ test on Kuhn.
- Each player's average is updated only in their own pass, with the strategy
  that pass used.
- Leduc's round logic is one function on the action list: a fold ends the
  hand, a call that isn't the opening check closes the round, otherwise the
  player to act is the list length mod 2. Chips per round fall out of a fold
  where a call matches the highest contribution and a raise adds the round's
  size on top.

Checks:

- 47 tests, 0.27 s.
- `test/reference_cfr.ml` is a slow second CFR that shares no code with the
  solver: per pass it enumerates the updating player's histories with their
  reach, evaluates every child with a separate expected-value walk, and adds to
  the strategy sum once per infoset (the solver does it once per history, which
  scales numerator and denominator alike). After 30 Kuhn iterations the
  average strategies agree to 1e-10 for all four variants, and a qcheck property
  does the same for 20 random (alpha, beta, gamma).
- Mutations, each re-run on the final code: weighting the average by t + 1
  fails 4 tests, discounting with t - 1 fails 3, simultaneous CFR+ fails 4
  (including OpenSpiel's CFR+ test and the Leduc solve).
- Leduc against OpenSpiel's `exploitability_test.py`: NashConv 4.747222222222
  for the uniform policy and 2 for always taking the first action, both exact
  to 1e-12. Tree sizes and ten payoffs worked out by hand.
- Leduc's value: CFR+ and DCFR at 10,000 iterations both give -0.085606 with
  exploitability under 1e-5, matching the -0.0856 usually quoted.

Speed (Windows laptop, native code), before and after the compiled tree:

| run                          | before | after  |
|------------------------------|-------:|-------:|
| Leduc, 10k iterations of CFR | 3.63 s | 0.17 s |
| Leduc, 10k of CFR+           | 7.47 s | 0.34 s |
| Kuhn, iterations per second  | 275k   | 1.92M  |

The benchmark puts vanilla CFR at 160 words allocated per Kuhn iteration and
5,524 per Leduc iteration. That's about 3 words per tree node: `walk` returns
a float, and OCaml boxes floats returned from a non-inlined recursive
function. Writing the value into a scratch slot instead of returning it
should remove nearly all of it.

Exploitability after 100,000 iterations (full curves in `docs/`):

| game  | cfr     | cfr+    | lcfr    | dcfr    |
|-------|--------:|--------:|--------:|--------:|
| Kuhn  | 6.76e-4 | 2.14e-6 | 8.36e-7 | 7.05e-7 |
| Leduc | 3.18e-3 | 4.73e-7 | 2.43e-4 | 1.10e-6 |

Linear CFR is the surprise: as good as the others on Kuhn, but on Leduc it
ends up only 13 times better than vanilla. My guess is the negative regrets:
linear CFR keeps them and only reweights them, so an action that looked bad
early takes a long time to come back, while CFR+ resets them and DCFR halves
them every iteration. Worth testing by sweeping beta.
DCFR leads on Leduc until about 8,000 iterations, then flattens out until about
30,000 while CFR+ keeps going.

MinGW's printf writes three-digit exponents, so the CLI now trims them and
prints `7.805e-06` on every platform. JSON output (the shortest `%g` that round-trips)
keeps the platform's format, which every JSON parser accepts.

Next: external-sampling MCCFR, then outcome sampling, then
their curves on the same charts. Removing the boxed
floats from `walk` is a quick win to try first.
