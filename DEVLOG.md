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

## 2026-10-22: Monte Carlo CFR

External-sampling and outcome-sampling MCCFR, exploitability charts against
nodes touched for all six algorithms, and walks that no longer allocate.

What exists now:

- `Mccfr.Make`: `External` and `Outcome {epsilon}` (OpenSpiel's 0.6 is
  `Mccfr.outcome`). `traverse t player` runs one sampled traversal, `iterate`
  runs one per player, and `split` copies the solver with an independent
  random stream (`Random.State.split`), which the tests use now and parallel
  MCCFR can use later.
- `Table`: the per-infoset flat arrays (regrets, strategy sums, the cached
  current strategy and its stamp) pulled out of `Cfr` so both solvers share
  them. `Tree.size` and `Tree.shape` size the scratch arrays.
- `touched` on both solvers counts nodes visited. `cfr-solver compare` now gives
  every algorithm the same node budget and plots exploitability against nodes
  touched, with the sampled methods dashed. `solve` takes `-a es`, `-a os` and
  `--seed`.
- The bench reports nodes per second next to iterations per second.

Design notes:

- External sampling never needs importance weights: the probability of
  sampling a history is the opponent's and chance's reach, which is exactly the
  weight counterfactual regret puts on it, so they cancel and the update is
  `v(a) - v` on sampled values.
- The opponent's strategy sum gets their current strategy added at every
  visit, with no weight. Its expectation is chance reach times the opponent's
  own reach. Full CFR weights by own reach alone, but for a fixed infoset the
  chance part is the same constant on every iteration, so it cancels when the
  sum is normalized. Outcome sampling divides by the traverser's sampling
  probability so the same thing holds.
- Outcome sampling returns one float per call: the payoff divided by the
  probability of the sampled path, times the strategy's probability of the
  path below the current node. The regret update is then `opp * x * (1 - p)`
  for the sampled action and `-opp * x * p` for the rest. Chance is sampled
  on-policy and left out of both the reach and the sampling probability.
- The cached strategy stamp works per traversal, the same as in full CFR.
  External sampling can hit one of the traverser's infosets many times per
  traversal, and the cache keeps the strategy fixed across those visits.
- Uniform draws come from `Random.State.bits` (30 bits) times 2^-30, so drawing
  doesn't allocate. 30 bits of resolution is far below any probability the
  solver produces.

The allocation fix flagged last time: `walk` returned a float and took three
float reach arguments, and OCaml boxes those across a non-inlined recursive
call. Reach now lives in a per-depth float array and the return value in a
one-slot float array. Vanilla CFR went from 5,524 to 2 words per Leduc
iteration and from 160 to 2 on Kuhn. Throughput did not change (about 57,000
Leduc iterations a second before and after, within noise): minor-heap
allocation was already cheap. I kept it anyway, because OCaml 5 minor
collections stop every domain, and parallel MCCFR is planned. Both MCCFR walks
allocate 0 words per iteration.

Checks:

- 61 tests, 4.4 s.
- Unbiasedness against exact expectations. From a solver state after 20 (Kuhn)
  or 50 (Leduc) iterations, split off 20,000 copies (4,000 for external
  sampling on Leduc), run one traversal on each, and compare the mean change of
  every regret and strategy-sum entry with the exact expected change from a
  separate full-tree walk in the test, within 5 standard errors. Both
  samplers, both players, both games. A few Leduc entries are hit about 50
  times in a million traversals, so the sample variance there is often zero;
  the standard error gets a floor from Var X <= max|X| E|X|.
- An exact invariant: summed with the current strategy's weights, one
  traversal's regret changes are zero at every infoset (qcheck, 200 random
  states per sampler).
- Mutation runs, each against the full suite: outcome sampling without the
  (1 - sigma) factor fails 4 tests, without the importance weight on the
  strategy sum 2, with the importance weight computed from sigma instead of the
  exploration mix 4, external sampling with a uniform opponent 4, and with a
  wrong strategy-sum increment 3. The third mutation first passed the
  unbiasedness tests: it makes NaNs, and `NaN > tol` is false. The checks are
  now written as `not (x <= tol)`.
- Same seed, same strategy to the bit; another seed gives a different one.
  Path lengths of a single Kuhn traversal are exactly {6, 8} nodes for
  external sampling and {4, 5} for outcome sampling.

Exploitability by nodes touched on Leduc (budget of 100,000 alternating
iterations, 387.8 million nodes):

| nodes       | cfr     | cfr+    | lcfr    | dcfr    | es      | os      |
|-------------|--------:|--------:|--------:|--------:|--------:|--------:|
| 10^5        | 3.05e-1 | 1.60e-1 | 2.28e-1 | 1.57e-1 | 5.91e-1 | 1.08    |
| 10^7        | 1.73e-2 | 7.50e-5 | 3.97e-3 | 3.35e-5 | 3.10e-2 | 9.29e-2 |
| 3.878 x 10^8| 2.03e-3 | 4.73e-7 | 2.43e-4 | 1.10e-6 | 4.79e-3 | 1.48e-2 |

Per node, external sampling ends 2.4 times more exploitable than vanilla CFR
and outcome sampling 7.3 times. On a 1,939-node game, sampling buys early
progress (2.5 exploitability after 1,000 nodes, before any full pass is done)
and nothing else. A million external-sampling iterations on Leduc take 0.59 s
and give value -0.0874 and exploitability 0.0119.

Speed (Windows laptop, release build): full-tree CFR about 100 Mnodes/s,
external sampling 76, outcome sampling 49. Sampling pays a random draw per
sampled node and jumps around memory more.

Next: parallel MCCFR on OCaml 5 domains, with one split solver per domain and
regrets merged or shared; then linear and discounted weighting inside MCCFR
(the averaging Pluribus used), to see how much of CFR+'s lead the sampled
methods can recover.

## 2026-10-22: Parallel MCCFR on OCaml 5 domains

Monte Carlo CFR now runs on several domains, in a shared-table mode and a
deterministic private-table mode, and the sampled solvers can be discounted
like linear CFR, DCFR and CFR+.

What exists now:

- `Parallel.Make (G)`: `create ~domains ?mode ?batch ?weighting`, `run`, and
  the same readers as `Mccfr` (`average_strategy`, `regrets`, ...). `Shared`
  (default) shares one regret table between domains with no locks; `Private`
  gives each domain its own tables and merges them every `batch` iterations
  per domain.
- `Barrier`: a reusable barrier on `Mutex` and `Condition` with a generation
  counter, so a fast domain can't run through the next phase's wait.
- `Rng`: SplitMix64 with its state in a 64-byte `Bytes`. Replaces
  `Random.State` everywhere in MCCFR, so every seeded result changed and the
  charts in `docs/` were regenerated.
- `Mccfr.create ?weighting ?interval`: discount regrets and strategy sums with
  the `Cfr` schedules (`Cfr.discounts algorithm t` gives the three factors)
  every `interval` iterations.
- CLI: `solve --domains D [--private] [--batch B] [-w lcfr|dcfr|cfr+]`, and
  `scale`, which runs serial, shared on each domain count and private on the
  largest, prints a timing table from one uninterrupted run each, and plots
  exploitability against iterations from a second run with checkpoints.
- The bench prints parallel external-sampling throughput up to the machine's
  recommended domain count.

Design notes:

- Private mode merges as `master + sum_j (table_j - master)`. Each domain
  merges its own stripe of slots between two barrier waits, then copies the
  master back. Worker 0's table goes first in the sum, so on one domain the
  merge is `master <- table_0` exactly and the result is bit for bit the
  serial solver. Workers are created with `Array.init` (left to right), and the
  merge always sums in domain order, so private runs are deterministic.
- Shared mode keeps the strategy sums per domain and folds them into the master
  at each sync point. They are write-only accumulators that never feed back
  into play, so sharding them changes nothing but the rounding. Regrets stay
  shared because the next strategy is read from them.
- Without weighting, shared mode doesn't sync at all until the end of `run`.
  With weighting both modes discount once per round, by stripes, between the
  barrier waits, so nobody writes while the discount runs.
- Each domain re-allocates its worker (record, scratch arrays, RNG and private
  tables) inside the domain at the start of `run`. OCaml 5 gives every domain
  its own minor heap and its own major-heap pools, so the hot data of two
  domains never ends up on one cache line.
- The current-strategy cache needs no invalidation after a merge: the stamp is
  the per-worker pass counter, which only grows, so the first visit of every
  traversal recomputes.
- Data races in shared mode are defined behaviour in OCaml 5. Float array
  elements are single 64-bit stores, so a racing read sees some written value;
  the only effect is an occasional lost increment.

The debugging, in order:

1. The first version gave 1.0x on 2 domains. The new `iterate` built a
   discount tuple on every iteration even when unweighted, and OCaml 5 minor
   collections stop every domain. Skipping it for unweighted runs: 1.45x.
2. Suspected false sharing next. Re-allocating workers inside their domains
   changed nothing by itself, but a microbenchmark of two domains drawing from
   `Random.State`s created back to back took 1.33 s against 0.45 s for one
   domain doing all the draws. The state is a bigarray whose 32 bytes come
   from `malloc`, so placement is luck. The SplitMix64 replacement also made
   serial external sampling faster (76 to 80 Mnodes/s); its tests check the
   published outputs for seed 1234567, which a Python version reproduces.
3. Still only 1.6x on 2 domains, and fully independent solvers in separate
   domains (no shared data at all) scaled exactly as badly. Pinning the process
   to one logical CPU per core (`start /affinity 5555`) gave independent
   solvers 1.99x on 2 and 3.6x on 4. Windows had been putting two domains on
   the two threads of one core.
4. Shared tables reached the same exploitability as serial per iteration at
   any domain count, but private tables did not. With 2 domains and a batch of
   1, private matched serial; with batches of 1,000 it ended 3 times worse
   after 4 million iterations, and with 10,000 twelve times worse after 1
   million. That fits the delayed-feedback bound for
   regret matching, which grows like sqrt(T x delay).
5. Shared x8 stopped at about 3.4x. Sharding the strategy sums took a 4M-iteration
   run from 0.67 s to 0.54 s.
6. `scale` first timed its checkpointed runs, and each `run` call costs about
   10 to 13 ms of domain start-up with 8 domains (40 chunks: 0.84 s against
   0.50 s for one chunk; spawning and joining 7 idle domains takes about 1 ms,
   so most of it is something else in starting real work). The table now
   comes from separate uninterrupted runs and the chart plots iterations.
   `scale` also spawns and joins its domains once before timing anything,
   because the first spawn in a process cost 16 ms on its own.

Results, Leduc, 4 million external-sampling iterations, Ryzen 7 7800X3D,
pinned to one logical CPU per core:

| config     | wall (s) | speedup | exploitability |
|------------|---------:|--------:|---------------:|
| serial     | 2.25     | 1.00    | 6.44e-3        |
| shared x2  | 1.30     | 1.73    | 6.14e-3        |
| shared x4  | 0.75     | 2.99    | 7.02e-3        |
| shared x8  | 0.47     | 4.75    | 8.09e-3        |
| private x8 | 0.33     | 6.83    | 5.45e-2        |

Unpinned the speedups are 1.64, 2.25, 3.68 and 4.00. Shared tables give the
best exploitability at any fixed wall time on Leduc. Private tables would look
better on a game whose table doesn't fit in L1, where contention drops and the
delay is small next to the run.

Weighting inside MCCFR, mean exploitability over 8 seeds, external sampling on
Leduc, discounting every iteration:

| iterations | none   | lcfr   | dcfr   | cfr+   |
|-----------:|-------:|-------:|-------:|-------:|
| 1,000      | 0.759  | 0.597  | 0.677  | 0.717  |
| 10,000     | 0.158  | 0.140  | 0.195  | 0.193  |
| 100,000    | 0.0467 | 0.0481 | 0.0634 | 0.0626 |
| 1,000,000  | 0.0129 | 0.0150 | 0.0207 | 0.0207 |

So the question from last time (how much of CFR+'s lead can the sampled
methods recover by weighting) has a short answer on Leduc: none. Linear
weighting helps early and fades; DCFR and CFR+ end 60% worse, plausibly
because cutting negative sampled regrets clips noise on one side only.
Discounting every 100 iterations instead of every one gave the same picture,
and so did discounting only the first 10,000 or 100,000 iterations (Pluribus
style): 0.0127 and 0.0117 against 0.0118 with none, mean of 4 seeds.

Checks:

- 81 tests, 4.7 s.
- Rng: published SplitMix64 outputs, the 30-bit draws for seed 1 (from the
  Python version), copy and split, and a uniformity check on `Rng.int`.
- Barrier: 4 domains through 500 phases, counting arrivals with an atomic; no
  domain ever sees fewer arrivals than its phase requires.
- Weighting: a qcheck property that discounting with the linear factors after
  every step leaves sum t x_t / (T + 1) of the raw increments, and DCFR's
  strategy factor leaves sum t^2 x_t / (T + 1)^2. CFR+ factors zero negative
  regrets. With an interval of 10, the first discount halves every entry of a
  plain run with the same seed, exactly. Linear weighting beats none at 1,000
  iterations over 8 seeds on Leduc (0.60 against 0.76).
- Parallel: one domain equals the serial solver bit for bit (regrets; strategy
  sums to 1e-9, since shared mode folds them in a different order) for both
  modes, both samplers, with and without weighting. Three private rounds on
  three domains equal a merge written out separately. Same seed, same private
  result. Iteration and round counts exact. Both modes converge on Kuhn and
  shared converges on Leduc (bounds checked against 10 to 20 seeds first).
- Mutation runs: merging without the master subtracted fails 2 tests, an
  off-by-one in splitting iterations 3, no copy back after a merge 3, a
  private master in shared mode 1, no per-round discount 1, and folding the
  shared strategy sums without resetting them 1.
- The serial Kuhn convergence test used to check seed 1 alone against 0.01.
  With the new generator seed 1 lands at 0.0101, the worst of 20 seeds (mean
  0.0060), so it now checks the mean of five seeds.

Next: a persistent domain pool, so `run` stops paying the start-up cost on
every call; then card abstraction (hand-strength buckets), which also gives
the parallel solver a game big enough that private tables should pay off.

## 2026-10-23: domain pool, card abstraction, hand evaluator

What was built:

- `Pool` (`lib/pool.ml`): worker domains spawned once and parked on a
  condition variable. `run pool d f` runs `f 0` on the caller and `f 1` to
  `f (d - 1)` on the workers, grows the pool if it needs more, re-raises the
  first exception, and `shutdown` joins everything. `Parallel` uses a shared
  default pool unless it is given one.
- `Leduc.game ?abstraction deck`: any number of ranks (up to 13) and suits,
  with infoset keys built from an abstraction (a label for the private card in
  round 1, and for card and board in round 2). The history type is shared by
  every instance, so a full game and its abstractions walk the same histories.
- `Abstraction.Make (Full) (Abs)`: walks the full game, maps each infoset to
  its abstract one (checking players, actions and that it really is a
  coarsening) and lifts abstract strategies back to the full game.
- `Strength`: hand strength of a Leduc card on a board, E[HS], E[HS^2] and HS
  histograms over boards. `Cluster`: optimal 1D k-means by dynamic
  programming, EMD on histograms, k-medians with k-means++ seeding. `Buckets`
  turns them into Leduc abstractions.
- `cfr-solver abstract`: 44 abstractions of a 13-rank deck solved with CFR+ on
  the pool, each scored by exact exploitability in the full game, with a CSV
  and two charts.
- `Hand`: card parsing and a 5 to 7 card hold'em evaluator.

Design notes:

- The pool's caller takes index 0, so a one-domain run never touches the
  workers and stays bit for bit the serial solver (the existing tests still
  check that). Workers that are not part of a call still wake on the
  broadcast and go back to sleep; with at most 15 of them that is cheaper than
  one condition per worker.
- `Parallel` copies each worker's state inside its domain only on the first
  run. Index j always lands on the same domain (tested), so the copy stays
  local. Before, every `run` re-copied.
- Shared mutable state is the price of the default pool: a second pool would
  mean more domains than cores. `Pool.run` holds a mutex for the whole call,
  so two callers queue instead of interleaving. Calling `run` from inside a
  job would deadlock; nothing does.
- Infoset keys stay strings. An abstraction only changes the label, so
  `Tree.build` gives the abstract game its own numbering for free and every
  solver works on it unchanged.
- 1D k-means works on distinct values (with summed weights), so equal
  strengths always share a bucket and k is capped at the number of distinct
  values. Bucket 0 is the weakest for every method, which makes the keys
  readable and the tests simple.
- k-medians instead of k-means for histograms: with EMD (L1 between CDFs) the
  mean of the CDFs is not the best center, the pointwise weighted median is,
  and it is monotone, so it is a CDF again. Each Lloyd step then lowers the
  cost and the loop ends at a fixed point.
- The first sweep had a bug: with round 2 "exact" the key used the round-1
  bucket instead of the card, so every round-1 abstraction also merged round
  2. The infoset counts gave it away (396 instead of 5,076 for one bucket). A
  test now checks that size on standard Leduc.
- The evaluator packs category and ranks into one int so comparing hands is
  integer comparison. Ranks seen once, twice, three and four times are four
  masks updated with `four |= thrice & b; thrice |= twice & b; ...`, which
  replaces a counts array. Lookup tables for popcount, top bit and straights
  (8,192 entries each) took it from 14.1 to 41.3 million seven-card hands a
  second.

Results (Ryzen 7 7800X3D, Windows):

- Pool: 4 million external-sampling iterations on 8 domains in 400 calls went
  from 15.85 ms a call to 1.88 ms (the work is about 1.63 ms a call). One
  uninterrupted run is unchanged (650 ms).
- leduc13 with 4,000 CFR+ iterations per abstraction, exact game at 3.1e-5.
  Round 1 with round 2 exact: one bucket 0.821; E[HS] 0.049 at 3 buckets and
  0.0097 at 10; E[HS^2] 0.0013 at 10; EMD histograms best at 2 and 4 buckets
  (0.170, 0.032) and worst from 6 on. E[HS] gets worse from 5 to 6 buckets.
- Round 2 by hand strength stalls near 0.1 with any number of buckets: HS on
  leduc13 has only 13 values, and card c with the board above has the same HS
  as card c + 1 with the board below. Imperfect recall (forget the card) gets
  0.094 at 438 infosets, better than perfect recall at up to 1,188. Keeping the
  board in the key and bucketing the card reaches the exact game.
- At 1,000 iterations instead of 4,000 the round 2 numbers moved by up to 20%
  but imperfect recall still beat perfect recall.
- All 133,784,560 seven-card hands give the published category counts and
  4,824 distinct values, at 41.3 million hands a second.

Checks:

- 112 tests, about 5 s. New: pool (indices, placement, 2,000 rounds,
  exceptions, shutdown), abstraction (identity, renaming commutes with CFR+
  exactly, lifted value equals abstract value for 40 random abstractions,
  refinements rejected), Leduc decks (closed-form tree sizes, contract walk on
  13x2 and 4x3), hand strength (by hand, mean 1/2 on 100 random decks, equal
  to check-down payoffs from the game itself), clustering (brute force over
  all labelings, EMD against sorted samples, median, blobs, fixed point), and
  the evaluator (all 2,598,960 five-card hands, best of 21 subsets, tricky
  hands).
- The shared-table Leduc test failed once in a full run (0.0411 against a
  bound of 0.04). Shared mode depends on how the domains interleave, so the
  same seed gives a spread: 40 runs on 4 domains ranged 0.024 to 0.039 with
  the pool and 0.022 to 0.037 at the commit before it. The pool didn't cause
  it; the bound was just at the tail. It is 0.05 now.
- Mutations caught: a pool that lets every worker run, a lift that drops part
  of the abstract strategy, ties counted as wins, round 2 labels transposed,
  k-medians picking the farthest center, an off-by-one in the k-means split,
  and a median at a third of the weight.

Next: limit hold'em river and turn subgames. That needs a public tree with
range vectors instead of one history per deal (1,081 hands a player on the
river), the evaluator above for showdowns, and hand-strength buckets for the
turn. Bet-size abstraction comes with it.

## 2026-10-23: hold'em subgames, bet sizes, action translation

What was built:

- `Range`: the 1,326 combos with a triangular index, range notation (AA, AKs,
  KQo, TT+, A9s+, AhKh, weights like QJs:0.5, random) and board removal.
- `Bet_tree`: public betting trees from a pot, an effective stack and per
  street bet sizes (pot fractions, chips or all-in), raise sizes, a cap and
  chance nodes between streets. `Bet_tree.limit` gives fixed-limit trees.
- `Subgame`: vector-form CFR on the public tree for river and turn spots. One
  regret and strategy-sum vector per decision node over the acting player's
  hands, reach and value vectors per player, the river dealt as a chance node
  on the turn. Vanilla CFR, CFR+, linear CFR and DCFR with the same schedule
  as `Cfr` (two helpers, `Cfr.alternating` and `Cfr.weight`, are now public).
  Exact best response and exploitability over the same tree, export to
  `Strategy.t`, and range-weighted action frequencies per node.
- `Translation` and `Subgame.translate`: nearest, arithmetic and
  pseudo-harmonic mappings, and a translated profile for a bigger tree.
- `cfr-solver subgame`, `translate` and `betsizes`, and bench entries for a
  river and a turn with random ranges. The CLI helpers shared by every command
  moved to `bin/report.ml`.

Design notes:

- Strategies are computed by regret matching when a node is visited, not
  stored. Each node is visited once per pass, so that equals refreshing at the
  start of the pass, and it saves a third of the memory. Vanilla CFR
  still updates both players in one walk, which is what makes it simultaneous;
  the other variants alternate. Scratch buffers are per depth, not per node.
- Showdowns are O(n) with card removal by inclusion-exclusion over 52 per-card
  sums, and the sorted order of each board is computed once, lazily, when the
  first showdown under it is built.
- Translation keeps one belief list per player: the player who acted follows
  the abstract action they actually chose; the other player maps the real
  action onto the abstract ones. The strategy at a node is the reach-weighted
  mixture over the actor's beliefs. Belief nodes that hit a terminal while the
  real game goes on are dropped. When an abstract action has no twin in the
  real tree (a re-raise clamped to all-in in one tree and not the other) the
  player bets the closest legal size, or calls.
- Unchecked index operators in the subgame hot loops only, declared as
  `external` on `float array` and `int array` so the float accesses stay
  unboxed. A local alias of `Array.unsafe_get` would be a generic array access
  with a tag check.

Results (Ryzen 7 7800X3D, one core, both ranges random):

- Fixed-limit river (pot 6, bet 1, 10 decisions): after 4,000 iterations,
  exploitability as a share of the pot is 4.4e-2% for CFR, 2.3e-3% for linear
  CFR, 4.2e-4% for CFR+ and 8.9e-5% for DCFR. 1,000 DCFR iterations take 0.61 s.
- Fixed-limit turn (pot 4, bet 1, 4,330 decisions): after 300 iterations
  CFR 0.82%, CFR+ 0.056%, linear CFR 0.047% and DCFR 0.018%. One solve with
  random ranges peaks at 523 MB working set: regrets and strategy sums are
  about 195 MB, and every exploitability checkpoint builds a full average
  profile. The first version of the command kept all four solvers alive until
  it printed, 2.1 GB; now only the last one is kept.
- Speed: 0.87 ms per river iteration and 401 ms per turn iteration at first;
  `-unsafe` on the whole library gave 0.63 and 294 ms; the unchecked
  operators with hoisted loop invariants 0.59 and 277 ms.
- Translation, bets of half pot and pot, x from 0.25 to 2 pots: worst case
  between the sizes is 0.17% of the pot for pseudo-harmonic, 0.36% for
  arithmetic and 2.4% for nearest. Outside the sizes they coincide, and a
  2-pot overbet costs 8.5%.
- Bet-size abstractions of a tree with seven sizes: without an overbet every
  abstraction is above 13% of the pot in the full tree; 0.25, 0.5, 1 and 2
  gets 0.26% with 26 decision nodes out of 44.

Checks:

- 141 tests, about 10 s. The vector solver against a scalar version of the
  same subgame (`test/subgame_ref.ml`) run through `Cfr.Make`: identical
  average strategies to 1e-9 after 25 iterations of CFR, CFR+ and DCFR on a
  river with overlapping ranges, ties and uneven weights; identical values and
  best responses on the river and the turn.
- The turn comparison failed twice, both times on exact ties. First, vanilla
  CFR split from iteration 2: KhKc on a jack river beats 7 of player 0's
  combos and loses to 3, and with uniform strategies bet and check are worth
  exactly the same when W/L = 7/3, so the regret is 0 and the two solvers
  round it to opposite signs. Uneven range weights removed that one. Then CFR+
  split from iteration 2 at KsKc facing a river bet after a 7 (the averages
  differed from iteration 4): the scalar solver had all regrets at zero and
  played uniform, the vector solver had a positive fold regret and folded. My
  reading, from the hands involved, is that every player 0 hand reaching that
  node held one of the kings, so the true compatible mass is 0 while
  inclusion-exclusion leaves a residue of rounding size. I did not isolate it
  further. The turn check now compares 3 iterations, plus the values and best
  responses as before.
- The clairvoyance game on real cards: value 1 and frequencies 1/2 within
  0.02. DCFR on the full random river drops monotonically to under 0.1% of the
  pot by 400 iterations.
- Mutations caught: no identical-combo term in folds (4 failures), ties as
  wins in the showdown (5), the wrong river-card probability (3), and naive
  averaging in translation (fails linearity).
- My first `extra size` test expected player 1's best response to stay put
  when a size is added. It went up, correctly: bet sizes are per street, not
  per player, so player 1 can use the new size too. The test now checks that
  both best responses can only rise and that the value is unchanged.

Next: suit isomorphism and turn trees that fit in memory with real ranges,
then hand-strength buckets on the turn and a river re-solve from public belief
states.

## 2026-10-27: suit isomorphism, float32, pruning, turn buckets

What was built:

- `Suit`: the 24 suit permutations, applying them to cards and combos,
  stabilizers and orbits of a set of cards.
- `Subgame.create ?iso ?single ?prune ?buckets`. `iso` (default on) finds the
  suit permutations that fix the board and both ranges' weights, deals one
  river (or turn) card per orbit and maps the other cards' values back through
  the permutation; `Subgame.symmetries` exposes the group. `single` stores
  regrets and strategy sums in float32 bigarrays. `prune` (default on) skips
  subtrees no opponent hand reaches. `buckets` (one count per street, 0 for
  exact) merges hands into E[HS^2] buckets on the turn and HS buckets on the
  river. `memory`, `skipped` and `bucket` report what happened.
- `Equity`: hand strength of every combo against a uniform random hand on a
  river in O(n log n) with card removal, and E[HS^2] on a turn.
- CLI: `subgame --single --no-iso`, a memory column in the header, and
  `cfr-solver buckets` for the sweep in `docs/buckets.svg`.
- Bench rows for float32, folding and pruning.

Design notes:

- Folding is exact only if the strategies above the chance node are
  symmetric, and floating point breaks symmetry: unfolded, on Ks 9s 5s 2s with
  real ranges, twin hands drift from 4e-16 after one iteration to 0.01 after
  five. So after every regret update each orbit of hands gets its mean regret,
  which makes the solver CFR on the game where symmetric hands share a
  strategy. That game contains an equilibrium of the real one by symmetry.
  Postflop-solver (read the source) swaps values the same way but doesn't tie
  the regrets.
- The first version of that averaging re-averaged orbits that already agreed,
  and (x + x + x) / 3 does not always round to x. Pruned and unpruned DCFR runs
  then differed in the last bit at iteration 3, which is how it showed up. Now
  an orbit whose regrets are already equal is left alone.
- Group per node: the root has the full group, below a river card c only the
  permutations that also fix c. Hand orbits are cached by (player, group), so
  thousands of nodes share a handful of arrays.
- The strategy export walks the tree with the composed suit permutation and
  writes every real card and hand, so a folded solve exports the same keys as
  an unfolded one and the tests can compare them directly.
- float32 is a variant of the regret store (`Double | Single`) with the two hot
  loops written out for each case; the cold paths go through `get` and `set`.
  The walk computes in doubles either way.
- Pruning: in an alternating pass the updating player's values are linear in
  the opponent's reach, so an all-zero opponent reach means zero values and no
  regret change below. The skipped subtree still gets a cheap walk that adds
  the updating player's strategy sums, so the result is identical to the bit.
  The best response skips such subtrees outright.
- Buckets live in the regret store, not the tree: a bucketed node keeps one
  row per bucket and a hand-to-bucket map, and everything else stays per hand,
  so `exploitability` on a bucketed solve is the exact one. Bucket regrets
  weight each hand by its range weight (the deal probability), which the
  scalar comparison requires. The average strategy weights hands by reach
  including the deal; `Cfr.Make` sums once per history instead, which under
  imperfect recall counts hands with more compatible opponents more, so the
  bucketed tests compare current strategies.

Results (Ryzen 7 7800X3D, one core):

| turn, both ranges random      | decisions | MB    | ms/it |
|-------------------------------|----------:|------:|------:|
| Qs Jh 8d 5c, double           | 4,330     | 203.2 | 243   |
| Qs Jh 8d 5c, float32          | 4,330     | 101.6 | 243   |
| Qs Js 8s 5s, unfolded         | 4,330     | 203.2 | 241   |
| Qs Js 8s 5s, folded           | 1,990     | 93.4  | 126   |
| Qs Js 8s 5s, folded, float32  | 1,990     | 46.7  | 127   |

MB counts regrets and strategy sums. float32 cost between 0.4% and 4% per
iteration across runs. Its strategies match double to 3e-8 after two
iterations and then drift at near-ties; exploitability stays within 1% at 30
iterations and was lower than double's at 100 (0.0039 against 0.0044 on a
monotone turn).

Pruning, real ranges on Qs Jh 8d 5c, DCFR iterations 101 to 200: 19.9 ms/it
without and 17.9 with, 529 subtrees skipped per iteration. With random ranges
nothing measurable: some hand reaches almost every line.

Buckets, Qs Jh 8d 5c, 230 hands against 477, 300 DCFR iterations,
exploitability in the exact tree as a share of the pot (exact: 0.014%, 63.7
MB): river buckets reach 0.065% at 32 (5.7 MB) and 0.042% at 64 (6.7 MB);
turn buckets save no memory, since the turn is one street against 44 rivers,
and reach 0.023% at 64. Nothing changes past 64 because no flush is possible
on a rainbow turn and many hands tie on strength.

Checks:

- 162 tests, about 19 s.
- Folded against unfolded on a monotone turn (CFR, CFR+, DCFR, 6 iterations)
  and a monotone flop (two nested chance levels): every infoset key the
  unfolded solver exports matches to 1e-9, plus value and both best
  responses. A folded strategy is bitwise equal across symmetric hands.
- Bucketed vector CFR against `Cfr.Make` on the bucketed scalar game:
  current strategies to 1e-9 for turn-only, river-only and both.
- Pruned against unpruned, 40 iterations of CFR+ and DCFR: averages, current
  strategies and best responses compared with `=`.
- `Equity.hs` against brute force on three rivers, E[HS^2] against the mean of
  brute-force HS^2.
- Mutations caught: permutation instead of inverse, images skipped in the
  best response, nested permutations composed in the wrong order (flop test
  only), no regret tying, unweighted bucket regrets, a uniform bucket strategy,
  float32 regret matching without the floor, float32 sums without the weight,
  no float32 discount, pruning without the sums, without zeroing the values,
  or checking the wrong player's reach.

Next: subgame re-solving from public belief states on the river, using the
turn solve's ranges at each river node; then real regret-based pruning (skip
negative-regret actions for as many iterations as they provably stay
negative), since skipping only zero-reach subtrees bought 10%.

## 2026-10-28: regret-based pruning

What was built:

- `Subgame.create ?rbp` (vanilla and linear CFR only): interval regret-based
  pruning from Brown and Sandholm, "Regret-Based Pruning in Extensive-Form
  Games" (NeurIPS 2015), in the vector solver. An action whose regret is
  negative for every live hand, so that regret matching gives it probability
  zero, is skipped for as long as a per-iteration bound proves its regret
  would still be negative, then caught up in one walk.
- `Subgame.pruned` and `caught_up` count skipped subtrees and finished
  windows. `Subgame.regrets` closes every open window and returns the
  cumulative regrets.
- CLI: `-a cfr-rbp,lcfr-rbp` and a pruned/it row under the timings.
- Bench rows for vanilla CFR with and without pruning on the limit turn.

Design notes:

- The bound. For hand h at iteration t the regret change is v(h, a) - v(h),
  and v(h) is known once the other actions are walked. v(h, a) is at most a
  sum over the opponent hands o reaching the node of the best payoff h can get
  against o below a. With a card still to come that is the largest payoff in
  the subtree. Without one it is the larger of the biggest fold payoff and the
  showdown payoff against o: the biggest showdown win if h beats o, 0 on a
  tie, minus the smallest showdown pot if h loses. That needs the opponent
  mass h beats and the mass that beats h, which two sweeps over the
  strength-sorted hands give with card removal, like the showdown itself.
  Over the first 512 iterations on the no-limit turn, windows lasted 3.3
  iterations on average with only the flat bound and 4.0 with this one.
- While an action is pruned, its window keeps the summed opponent reach, the
  summed node value and the summed bound, each divided by the running
  discount product D. Under vanilla CFR D is 1. Under linear CFR every regret
  is multiplied by t/(t+1) each iteration, so a contribution from iteration s
  is worth D(t-1)/D(s-1) of itself at iteration t, and dividing by D(s-1) on
  the way in and multiplying by D(t-1) on the way out does that exactly.
  CFR+ floors negative regrets and DCFR(1.5, 0, 2) halves them, both
  sign-dependent, so the catch-up stops being linear and RBP is refused.
  Discounted with alpha = beta would be uniform but can drive D to zero
  (alpha = 0 halves it every iteration), so it is refused too.
- Why one walk catches up the whole window: nothing below a pruned action
  changes while it is pruned. The acting player's regrets there are not
  updated (that is the point), the opponent's are updated with the acting
  player's reach, which is zero, and linear CFR scales every row by the same
  factor, which regret matching ignores. With the strategies below frozen,
  every counterfactual value below is linear in the opponent's reach at the
  top, so the sum over the window is one walk with the summed reach.
- The catch-up plays a counterfactual best response below the action, as in
  the paper: each of the acting player's nodes takes the max over actions,
  and every action there gets regret D (V(a) - max), never positive. When the
  subtree has no decision of the acting player the best response has nothing
  to choose, so the run has to equal plain CFR up to rounding. That is the
  main test.
- The opponent's strategy sums below a pruned action are linear in its reach
  at the top too, so they are deferred as one more summed vector and added in
  one pass when the window closes or `average` is called. For speed in the
  simultaneous vanilla pass this mattered more than anything else: before it,
  a skipped subtree was still walked once for the opponent's sums, and
  pruning came out slower than not pruning.
- A window nested inside another one stays open when the outer one catches
  up. The two cover disjoint iterations, so the outer best response and the
  later inner catch-up add up correctly in either order. The first version
  closed inner windows from inside the outer catch-up; a mutant without that
  passed every test, and on reflection it was not a bug, so it went.
- Hands blocked by a card already dealt never hold a window open. Hands with
  zero reach do, because their regrets still change.
- Tried and dropped: pruning only subtrees that contain a chance node. On the
  no-limit turn those are about 1% of what gets pruned, and their catch-up
  walks cost what the skips save.

Results (Ryzen 7 7800X3D, one core; back-to-back timings vary by about 10%,
so the limit turn alternates the two solvers in blocks of 100 iterations):

| vanilla CFR unless noted                         | pruned/it | plain  | RBP    | exploit plain / RBP |
|--------------------------------------------------|----------:|-------:|-------:|--------------------:|
| limit turn Qs Jh 8d 5c, real ranges, 2,000 it    | 320       | 26.4 s | 23.4 s | 0.194% / 0.199%     |
| no-limit turn, 3 sizes + all-in, 1,000 it        | 383       | 22.3 s | 20.8 s | 2.64% / 2.60%       |
| same, linear CFR                                 | 293       | 31.1 s | 31.4 s | 0.111% / 0.120%     |
| no-limit river, random ranges, 4 sizes, 3,000 it | 2.1       | 4.10 s | 4.54 s | 0.501% / 0.493%     |

On the limit turn every block after iteration 400 was faster with pruning,
13.0 against 11.2 ms an iteration at the end. A single bench sample of
iterations 1,001 to 1,100 had it the other way (12.7 against 14.7 ms), which
is why the README quotes the interleaved run. With pruning off, the old and
new code time the same within noise on the random-range limit turn.

The windows explain the small gain. A shared subtree can only be skipped when
every hand in the range has given the action up, so a window is the minimum
of all the hands' windows. On the no-limit turn they averaged 4 iterations over
the first 512, and every one ended on the bound, never because some hand's
probability went positive. On a no-limit river with the test ranges they grow
with t, as the theory says: 7 iterations at 1,000, 21 at 16,000. Over those
16,000 iterations the exploitability ratio against plain CFR wanders between
0.81 and 1.18 for both CFR and linear CFR and ends lower.

Checks:

- 168 tests, about 22 s.
- One-bet river, r1 against r0 (every prunable action there leads only to
  opponent decisions): 400 iterations of CFR and of linear CFR, 294 subtrees
  pruned and 60 windows caught up; averages and best responses match plain
  CFR to 1e-9 and every regret to 1e-6. With the ranges the other way round,
  player 0 bets every hand by iteration 98, the check line (which holds player
  0's own call or fold) gets pruned, the best response kicks in and the runs
  part, as they should. Found by bisecting iterations.
- No-limit river with raises: exploitability after 2,000 iterations within a
  third of plain CFR's for both algorithms.
- Folded against unfolded with pruning on a monotone turn, 60 iterations:
  every exported infoset to 1e-9.
- Reading `average` every iteration leaves the regrets bit for bit the same
  as never reading it.
- Mutations caught: dropping the pruned action's own regret at catch-up
  (caught only once the regrets were compared; a strategy can't show a
  negative regret that is not negative enough), no bound, a bound with the
  base payoff too low, linear CFR without the scale, deferred strategy sums
  that are dropped or never flushed, and a mean instead of a best response in
  the catch-up. Not caught: leaving tied hands out of the bound (too tight,
  but never decisive in the tests) and dropping the symmetrization after a
  catch-up (rounding level).

Next: subgame re-solving from public belief states on the river with the
turn solve's ranges, then EMD turn buckets from histograms of river hand
strength against the E[HS^2] curve in docs/buckets.svg. Pruning per hand
instead of per subtree would need a different walk and isn't worth it at
these sizes.

## 2026-11-03: distribution-aware turn buckets, river re-solving

What was built:

- `Equity.rivers`: for every combo on a turn, its hand strength on each of the
  46 rivers it can see, sorted. With `~quantize:k` each river's values are
  first replaced by the mean of their optimal 1D k-means cluster on that
  river. `Equity.histogram ~bins` bins the same values. `Equity.ehs2` is now
  the mean of squares of the sorted distribution.
- `Cluster.quantile_kmedians` and `Cluster.wasserstein`: k-medians on sorted
  samples under L1, which is exact EMD between equal-size distributions.
  `kmedians` and the new function share one fitting routine, and the seeding
  keeps a running nearest-center distance instead of recomputing it for every
  new center (same draws, same labels on the Leduc decks, checked before and
  after).
- `Holdem_buckets`: turn features `ehs2`, `histN`, `emd` and `potN`, with
  identical distributions merged and weighted before clustering and labels
  made contiguous after. `Subgame.create ?feature` picks the turn feature;
  rivers stay on hand strength.
- `cfr-solver buckets --features ehs2,hist50,emd,pot8 --turn-only` and the two
  charts `docs/turn-buckets.svg` (rainbow) and `docs/turn-buckets-twotone.svg`.
- River re-solving in `Subgame`: `roots` lists every river root of an
  unfolded turn tree with the pot and both players' reach under a profile;
  `restrict` copies a turn profile onto a river tree and `graft` writes a
  river profile back for one player; `cbv` gives per-hand counterfactual
  best-response values; `gadget t opp alt` turns a river solve into the
  re-solving game where `opp` may take `alt` instead of playing; `resolve
  ~safe` re-solves every river and returns the stitched profile.
- `cfr-solver resolve`: blueprint at several iteration counts, then unsafe and
  safe re-solving of every river, all measured in the exact turn tree.

Design notes:

- Why sorted vectors. Each hand on a turn sees exactly 46 equally likely
  rivers, so its distribution of river strength is 46 numbers. In one
  dimension W1(F, G) is the integral of |F^-1(u) - G^-1(u)|, which for two
  sorted vectors of the same length is the mean absolute difference: EMD
  turns into L1. The weighted median of each coordinate minimizes the summed
  L1 to the members, and medians of sorted vectors stay sorted, so Lloyd's
  algorithm in this space is exact EMD k-medians. The histogram version
  rounds every value into a bin first; the test that ties the two together
  checks `wasserstein` against `emd` on grid samples, and another checks it
  against brute force over all assignments.
- Potential-aware with one-dimensional river clusters. Ganzfried and Sandholm
  describe the turn feature as a histogram over river clusters with the
  distance between clusters as the ground distance. When the river clusters
  are intervals of hand strength, that distance is the distance between their
  means, and EMD with that ground metric is again a 1D Wasserstein distance:
  replace each river value by its cluster mean and use the sorted vectors.
  With multi-dimensional river features (opponent cluster hand strength) it
  would need a real transport solver.
- Dedup. A full range on Qs Jh 8d 5c is 1,128 combos and 86 distinct
  distributions, on Qs Jh 8s 5c 274. Clustering the distinct points with
  multiplicities is the same objective and makes every label call take 20 to
  110 ms. It also exposed the E[HS^2] rounding: summed over rivers in deck
  order, two hands with the same multiset of values could differ in the last
  bit (87 distinct values for 86 distributions), and optimal 1D k-means would
  happily cut between them. Summing the sorted values fixes the order. None of
  the existing bucket results changed.
- The gadget (Burch, Johanson and Bowling, "Solving Imperfect Information
  Games Using Decomposition", AAAI 2014). For each river root, re-solve the
  river with both players' blueprint ranges, but give the opponent a choice
  before it starts: play, or take alt(h), its counterfactual best-response
  value against the blueprint on that river. In the vector solver that is one
  extra two-action regret matcher per opponent hand. Each iteration the
  opponent's root reach is its range weight times the probability of
  playing, and after the opponent's pass its root values v(h) update the two
  regrets against u = p v + (1 - p) alt (floored at zero, as in CFR+). The
  resolver's regrets inside the river only ever see the opponent through that
  reach, which is exactly the gadget game. At an equilibrium of it the
  opponent's best-response value against the new river strategy is at most
  alt(h) for every hand it holds, so no opponent hand does better than
  against the blueprint, and the stitched strategy is no more exploitable.
- With no positive regret the gadget plays. Any choice is fine for regret
  matching there, and this one makes a gadget with very low alternatives
  reproduce the plain solve bit for bit, which is a test.
- Units. alt and v are both computed inside the same river solve, against the
  same resolver reach, so the river's own payoff convention (half the pot at
  the root) never has to match the turn tree's. The pot and the remaining
  stack come from the turn tree's river root.
- Each player is re-solved separately (the gadget is for the other one), and
  only the resolver's strategy is grafted back. Unsafe re-solving is one
  solve per river and grafts both.
- `roots` refuses folded trees. A folded river child stands for several real
  cards whose reaches are permuted copies, so the blueprint would have to be
  unfolded to graft per card anyway.

Results (Ryzen 7 7800X3D, one core). Turn buckets only, rivers exact, the
README ranges (230 against 477 hands), 300 DCFR iterations, exploitability in
the exact tree as a share of the pot:

| buckets | Qs Jh 8d 5c ehs2 | emd    | Qs Jh 8s 5c ehs2 | emd    |
|--------:|-----------------:|-------:|-----------------:|-------:|
| 2       | 4.05             | 5.02   | 4.49             | 4.77   |
| 8       | 1.25             | 0.790  | 1.13             | 0.916  |
| 16      | 0.434            | 0.233  | 0.525            | 0.259  |
| 32      | 0.123            | 0.0424 | 0.269            | 0.129  |
| 64      | 0.0233           | 0.0233 | 0.105            | 0.0533 |

50-bin histograms land close to exact EMD (EMD ahead at 7 of 12 sizes), and
potential-aware over 8 river clusters loses ground at 4 and 16 buckets. The
full tables are in the README.

River re-solving on Qs Jh 8d 5c, 432 river roots, 500 DCFR iterations per
river solve:

| blueprint | exact blueprint | unsafe | safe   | 16 river buckets | unsafe | safe  |
|----------:|----------------:|-------:|-------:|-----------------:|-------:|------:|
| 10 it     | 10.3            | 3.03   | 6.64   | 11.9             | 4.38   | 8.67  |
| 30 it     | 0.848           | 0.392  | 0.677  | 1.12             | 0.681  | 0.898 |
| 100 it    | 0.0898          | 0.0844 | 0.0702 | 0.241            | 0.270  | 0.185 |
| 300 it    | 0.0140          | 0.0312 | 0.0119 | 0.224            | 0.495  | 0.167 |

Safe re-solving never made a blueprint worse here; unsafe re-solving made both
300-iteration blueprints 2.2x more exploitable. On the small test turn with
only 200 iterations per river, safe re-solving of a 300-iteration blueprint
came out at 0.0070% against the blueprint's 0.0056%: the guarantee is for a
solved gadget game, and 200 iterations is not that. The safe pass takes 88 to
119 s per blueprint.

Checks:

- 191 tests, about 42 s.
- Leduc k-medians labels identical before and after the Cluster refactor
  (Hist 5 and Hist 10 at 2, 3, 5 and 8 buckets on a 13x2 deck), and the
  README bucket sweep reproduces docs/buckets.csv exactly with the new
  E[HS^2].
- EMD on sorted samples against brute force over all assignments; against
  binned EMD on grid samples; quantile k-medians fixed point and ordering.
- River distributions against brute-force hand strength per river.
- Re-solving: roots against `lines` masses and hand-computed pots, restrict
  then graft is the identity, CBVs against the best response, the gadget
  with low alternatives bit for bit equal to a plain solve, the per-hand
  gadget bound after 2,000 iterations (worst gap exactly 0 across 8 solves),
  and safe re-solving lowering exploitability of 1-, 10- and 100-iteration
  blueprints.
- Mutations caught: a gadget that always plays (2 tests), alternatives from
  the blueprint's mean values instead of best-response values (2 tests; the
  end-to-end safety test alone passes with it).

Next: max-margin and reach gadgets next to this one on the same turn, then
nested re-solving (turn from a flop blueprint, river from the re-solved
turn), and potential-aware buckets with multi-dimensional river features,
which needs a real transport solver for EMD.

## 2026-11-06: max-margin and reach gadgets

What was built:

- `Subgame.gadget` takes a kind: `Resolve` (the opt-out from last time) or
  `Maxmargin`, where the opponent chooses which hand to start the river with
  through one RM+ regret matcher over its hands and is paid v(h) - alt(h)
  per unit of reach. `alternatives` and `margins` read a gadget back.
- `Subgame.gifts`: for each river root of a turn tree, per opponent hand, the
  sum over the opponent's turn decisions on the way there of
  max(0, fold value - value of the action taken) / m, where m is the
  resolver's reach that does not collide with the hand at that decision.
- `Subgame.solving = Unsafe | Safe of gadget | Reach of gadget` and
  `Subgame.prepare`, which builds the augmented river game for one root and
  one resolver; `resolve` is now a loop over `prepare`.
- `cfr-solver resolve --methods` runs any subset of the five and plots them.
- A fix to the 11-03 gadget: it only covered opponent hands with positive
  blueprint reach at the river.

Design notes:

- Entry distribution. Burch, Johanson and Bowling's gadget starts with chance
  picking the opponent's information set in proportion to pi_-1, the
  probability that the opponent reaches it if it tries. The 11-03 version
  used the opponent's blueprint range instead. That looks equivalent (at an
  equilibrium every hand with positive weight is held to its alternative,
  whatever the weight) except for hands with weight zero: they were not in
  the river game at all, so nothing held them down. The river game now gets
  every hand of the opponent's turn range with weight 1 (in the vector
  solver pi_-1 is the resolver's reach times chance, which the walk already
  carries). The test that shows it makes the blueprint fold 77 and AK to a
  bet at "b1" on Ah Kd 7c 2h. With the old gadget the sets' counterfactual
  value at the turn root goes from 37.1 to 46.7 after "safe" re-solving; with
  the new one every hand loses value. On the README turn the resolve column
  moved by at most 4.2% either way, so the 11-03 numbers were not much off,
  but the guarantee was not there.
- Max-margin in vector form. Brown and Sandholm describe the gadget game as
  the opponent picking an information set, then chance picking the node in
  proportion to pi_-1, with every payoff shifted by the alternative. For a
  hand h with non-colliding resolver mass m(h), the opponent's root reach is
  q(h) / m(h), so the joint over (h, resolver hand) is q(h) times the
  conditional, and the regret matcher's utility for h is
  (v(h) - alt(h)) / m(h), where v is the counterfactual value the walk
  returns. The resolver's regrets only see the opponent through that reach,
  as with the opt-out. Hands with m = 0 cannot be picked. Dividing by m is
  what makes it a margin per unit of reach; without it max-margin would
  defend hands in proportion to how often they meet the resolver.
- Why gifts are safe. Write B(X, h) for the opponent's best-response
  counterfactual value at node X against the blueprint, New for the same
  after re-solving, M(X, h) for the non-colliding resolver mass at X and
  G(X, h) for the gifts per unit of reach from the opponent's decisions
  above X. Claim: New(X) <= B(X) + G(X) M(X). At a river root the gadget
  gives exactly that (alt = B + G m). Through a chance node New is the
  1/44-weighted sum over rivers, and the river masses sum back to M with the
  same weight (every resolver hand that misses h sees 44 rivers that miss
  both), so the bound survives. Through a resolver node New and M are sums
  over its actions with its strategy inside the reach. At an opponent node
  New is a max: a fold child is unchanged, and an action a gets
  G + (B(fold) - B(a))^+ / M, so its bound is
  max(B(a), B(fold)) + G M <= B(X) + G M. At the turn root G = 0, so no
  opponent hand gains anything. The same induction works with gifts from
  every alternative action, which is why that mutant survives. Brown and
  Sandholm use only actions that lead straight to a terminal because those
  values cannot shrink when other subgames are re-solved, so the gift stays
  a lower bound and their Theorem 1 improvement guarantee holds.
- The split gives each river a share proportional to the resolver's reach
  into it: the same gift per unit of reach everywhere, the analogue of their
  equal split. With 44 rivers under each chance node that is a small share,
  and the gifts barely moved anything (below). They saw the same with 1,755
  flops and got a lot more by not splitting.
- Units again. Turn-tree values at a river root before the chance weight are
  in the same units as the river game's values (the bet tree pays half the
  current pot plus what was put in, and the river game starts from the root's
  pot and stack), so a gift per unit of reach times river mass goes straight
  onto alt. The gifts test checks that end to end: B(call) summed from 48
  river games against the turn tree's own gift.

Results (Ryzen 7 7800X3D, the exact and bucketed runs side by side, one core
each). Qs Jh 8d 5c, README ranges, 500 DCFR iterations per river, % of pot:

| blueprint | exact  | resolve | maxmargin | reach-mm | 16 buckets | resolve | maxmargin | reach-mm |
|----------:|-------:|--------:|----------:|---------:|-----------:|--------:|----------:|---------:|
| 10 it     | 10.3   | 6.67    | 4.48      | 4.48     | 11.9       | 8.67    | 6.79      | 6.79     |
| 30 it     | 0.848  | 0.654   | 0.569     | 0.563    | 1.12       | 0.889   | 0.795     | 0.790    |
| 100 it    | 0.0898 | 0.0689  | 0.0596    | 0.0587   | 0.241      | 0.183   | 0.165     | 0.162    |
| 300 it    | 0.0140 | 0.0124  | 0.0105    | 0.0101   | 0.224      | 0.167   | 0.152     | 0.152    |

Max-margin beat resolve at every point, by 9 to 33%. Reach gifts took 6% off
resolve and 3% off max-margin at 300 exact iterations and under 2% anywhere
else; at 10 exact iterations reach-resolve was 0.04% worse than resolve.
Kubicek, Lisy and Sandholm (IJCAI 2026) find resolving beating max-margin in
their benchmarks and trace it to which of the gadget game's many equilibria
a solver lands on. Here DCFR on the vector tree lands on max-margin
solutions that are better, but this is one turn and one solver. The unsafe
and blueprint columns match 11-03 to three digits. Each gadget pass is 80 to
110 s for 864 river solves.

Checks:

- 195 tests, about 130 s (the four new ones are slow: 2,000 to 3,000
  iterations per river).
- Max-margin: after 3,000 iterations the smallest margin is above -1e-3 and
  never below resolve's, on 8 river solves.
- Gifts: zero on check-check lines, non-negative everywhere, and on the
  "b1c" line equal to max(0, -2 M - B(call)) with M and B(call) built from
  the 48 river games, to 1e-9 of the value range; the resolver mass from the
  rivers matches the mass from the turn strategy to 1e-12, and the raised
  alternatives sum back to the gift.
- Per hand at the turn root, for all four gadgets after 2,000 iterations
  per river on a 30-iteration blueprint: no opponent hand gains more than
  1e-4 of the value range. Same with the blueprint folding its sets.
- Mutants caught: gifts not split (the gifts test), the old range-weighted
  entry (the folded-sets test), plain RM in the chooser (3 tests). Gifts from all actions survive, as they
  should.
- The toolchain had lost `dune.exe` from the opam switch (opam still listed
  dune 3.24.2 as installed); `opam reinstall dune` rebuilt it and its
  dependents.

Next: nested re-solving (turn from a flop blueprint, river from the
re-solved turn), gifts scaled up past the safe split to see where it stops
helping, and multi-dimensional potential-aware buckets.

## 2026-11-07: Liar's dice, playing the solver

What was built:

- `Liars_dice`: one round of Liar's dice for any number of dice per player
  and 2 to 9 faces, with the top face wild or not. Bids are numbered
  (quantity - 1) * faces + (face - 1), so "higher bid" is just a larger
  index (OpenSpiel's reset-face rule). Rolls are dealt as sorted multisets
  with multinomial probabilities, and an infoset is the player's own roll
  plus every bid so far (`3:1x2,1x4`). `Liars_dice.game config` returns a
  first-class `Game.S`, so every solver, the best response and the CLI work
  on it unchanged. The CLI has `liars` (1 die, 6 faces, sixes wild, the
  OpenSpiel default), `liars-nowild` and `liars-2d3`.
- `Play.Make (G)`: deals one hand against a strategy, sampling chance and the
  bot from an `Rng`, and asks a callback for the seat's moves. `cfr-solver
  play` wraps it in a terminal loop (alternating seats, running score).
- `solve --show KEY` prints chosen infosets of strategies too big for the
  full table.
- `Rng.float` and `Rng.draw` (weighted index), taken out of `Mccfr` and
  `Cluster`, which each had their own copy.

Design notes:

- Size. Bids only go up, so the bids made so far are a subset of the 12
  possible bids and every subset happens: 2^12 public states, each split by
  the 6 private dice, 24,576 infosets, OpenSpiel's count. The full tree is
  1 + 36 (2^13 - 1) = 294,877 nodes, and the tests check that closed form on
  four other variants. Two dice of six faces would be 21 x 2^24 infosets per
  player, which is out of reach for a full tree; 2 dice of 3 faces is the
  same size as the standard game.
- Independent check. A Python script (kept outside the repo) builds the
  sequence-form LP from the rules, with ordered rolls instead of multisets
  so it shares nothing with the OCaml deal code, and solves it with HiGHS in
  about 10 s for all 14 variants. The standard game is -7/258 = -0.0271318
  for the first player. One 2026 arXiv paper on self-play collapse (Kujur) lists about -0.076
  for a one-die, no-wild version; both the LP and CFR give -1/9 there, so
  either their rules differ somewhere they don't state or the number was not
  converged.
- The 1-die, 2-face game without wilds converges only like 1/T under every
  variant (DCFR is at 5e-4 after 1,000 iterations, while 1 die of 3 faces is
  at 4e-8). I have not tracked down why.
  The test bound for that one variant is 1e-3; the others are held to 1e-6
  (1e-4 for 1 die of 4 faces with wilds, 2.6e-5 at 1,000 iterations).
- Play reports the whole move list after the hand, with each move's
  infoset, which is how the bot's die becomes visible without the game
  interface having to expose private information.

Results (Ryzen 7 7800X3D, one core):

- 1d6 with DCFR: 8.0 ms per iteration, 5,000 iterations in 39.9 s, value
  -0.027132, exploitability 2.2e-6. 1d6 without wilds: -0.111111 and 6e-9
  after 2,000. 2d3: 0.061728 (5/81) and 5.8e-6.
- Same-work comparison at 1.18e9 nodes (table and chart in the README):
  DCFR 1.27e-5, CFR+ 3.31e-5, LCFR 5.39e-4, ES 2.40e-3, CFR 7.11e-3,
  OS 9.76e-3. External sampling beats vanilla CFR here, the reverse of
  Leduc.
- Opening strategy (one equilibrium): a 5 always opens 1x5, a 4 opens 1x4
  93% of the time, a 3 opens 1x3 70% and bluffs 1x4 or 1x5 the rest, a wild
  six spreads over 1x3 to 1x5, and a 1 and a 2 play identically.

Checks:

- 209 tests, about 121 s. New: tree shape for four variants, OpenSpiel's
  infoset count, roll probabilities (21 multisets of 2d6, 6/64 for three
  distinct d4), bid truth with and without wilds, hand-checked payoffs,
  infoset keys, legal bids, LP values for 8 variants and -7/258 (slow).
- Play: two copies of Kuhn's equilibrium over 200,000 sampled hands average
  -1/18 and +1/18 by seat within four standard errors; the callback is only
  asked at the seat's own infosets with the legal labels; seeded runs
  replay; an unknown label raises.
- `Rng.draw` frequencies on weights 1, 0, 3, 4 over 400,000 draws. MCCFR
  and clustering tests unchanged after the move, so the sampling is
  bit-identical.
- CI smoke-tests `solve -g liars-2d3 --show` and `play` on piped input.

Next: three-player Kuhn poker (needs the game interface to go past two
players), then the gift-scaling experiment on the river re-solver and
nested re-solving from a flop blueprint.

## 2026-11-07: three players

What was built:

- `Multi`: a game signature for any number of players. Terminals carry a
  payoff vector, decisions name a player index. `Multi.Of_game` lifts a
  two-player `Game.S` into it and `Multi.Eval` gives every player's expected
  value under a profile.
- `Multi_br`: best response and NashConv for n players (each player's gain
  from deviating, summed). `Best_response` is now a thin wrapper over it, so
  the two-player brute-force and OpenSpiel tests exercise the same code.
- `Multi_cfr`: CFR, CFR+, LCFR and DCFR over `Multi.S`, with the same
  schedules and alternation as `Cfr`. Alternating variants make one pass per
  player.
- `Kuhn_n.game n` (n + 1 cards, OpenSpiel's n-player rules) and
  `Leduc_n.game ?ranks n` (n + 1 ranks by default, two suits, raises of 2 and
  4, two raises a round, folded players skipped).
- `cfr-solver multi -g kuhn3|kuhn4|leduc3|leduc3-6`: runs the four variants,
  prints NashConv at checkpoints, each player's value and gain, and writes a
  CSV and a log-log chart.

Design notes:

- Counterfactual reach for player p is the product of every other player's
  reach and chance's. The reach array has n + 1 slots and the walk multiplies
  one slot in place and restores it, so a decision costs no allocation.
- First version returned a fresh utility vector (and an array of action
  values) from every node: 0.204 ms per iteration on two-player Leduc against
  0.040 for `Cfr`, and 40.2 ms on eight-card three-player Leduc. With flat
  regret tables and per-depth buffers (`vals` holds one utility vector per
  depth, the child writes at depth d + 1 and the parent folds it into d) it
  is 0.055 and 12.2 ms, identical output.
- Leduc's round closes when no one has raised and everyone left has called,
  or after a raise when everyone left but the raiser has called. A call
  counter that resets on each raise does both, and folds only shrink the
  number of players left. That is how OpenSpiel does it.
- The infoset keys keep the two-player format (`K:cr`, `KQ:cr/c`), so
  `Leduc_n.game 2` and `Kuhn_n.game 2` give exactly the old games and the
  tests can compare the two solvers strategy by strategy.

Results (Ryzen 7 7800X3D, one core):

- Three-player Kuhn (48 infosets): NashConv after 100,000 iterations is
  1.0e-3 for CFR, 1.5e-7 CFR+, 4.2e-7 LCFR, 6.1e-10 DCFR. The second player
  gets -0.020833 (-1/48) and the others match Szafron, Gibson and
  Sturtevant's family at beta = 0.126; the first player never opens with a
  bet. CFR+'s beta wanders (0.138, 0.136, 0.145 at 1k, 10k, 100k) while
  NashConv keeps falling.
- Six-card three-player Leduc (4,203 infosets, 1,000 iterations): CFR+ 4.7e-3,
  DCFR 1.3e-2 and flat since 500, LCFR back up from 0.137 to 0.155, CFR 0.50.
  CFR+ and DCFR end at different approximate equilibria: the second player
  loses 0.195 a hand under one and 0.026 under the other.
- Eight-card three-player Leduc (7,428 infosets, 10,000 iterations): DCFR
  3.0e-4, CFR+ 1.7e-3 and slowing, LCFR stuck near 2e-2 from 1,000 on, CFR
  5.1e-2. Here CFR+ and DCFR agree on the values to 3e-4. 10.5 ms an
  iteration for the alternating variants.

Checks:

- 225 tests, about 133 s. New: the n-player solver on Kuhn and Leduc (native
  `Kuhn_n 2`/`Leduc_n 2` and through `Of_game`) against `Cfr`, all four
  variants, strategies within 1e-11; three-player Kuhn sizes (600 histories,
  312 terminals, 16 infosets a player, OpenSpiel's 48); hand-worked payoffs
  for both games including a split pot and a fold skipped in the second
  round; legal actions at the raise cap; Leduc sizes with suits told apart
  against Lu, Zhu and Zhao's table (9,450/936 and 396,120/13,878); best
  response against a per-card brute force over pure strategies (qcheck); and
  the Szafron family checks after 2,000 DCFR iterations.
- I couldn't open the AAMAS paper itself (the university link redirects to a
  dead page). The payoffs -(1 + 2 beta)/48, -1/48 and (1 + beta)/24 come from
  the summary on Wikipedia; they sum to zero, and the solver reproduces the
  constant -1/48 to six digits.
- Charts rendered and looked at: docs/kuhn3.svg, docs/leduc3.svg,
  docs/leduc3-6.svg.

Next: external-sampling MCCFR for n players, then a closer look at which
equilibrium each algorithm picks in three-player Leduc, then the gift-scaling
experiment on the river re-solver.

## 2026-11-07: external sampling for n players, which equilibrium

External-sampling MCCFR now runs on the n-player signature, NashConv is fast
enough to measure four-player Leduc, and there is a first look at which
equilibrium each method ends up in on three-player Leduc.

What was built:

- `Multi_tree`: the flat tree (`Leaf`, `Chance`, `Decision` with an infoset
  index) and the infoset tables, pulled out of `Multi_cfr` so the sampler and
  the best response can share it. `Multi_cfr` builds on it, unchanged
  otherwise.
- `Rng.pick`: sampling an action from a slice of a probability array, moved
  out of `Mccfr` so both samplers use the same draw.
- `Multi_mccfr`: one pass per player; the traverser enumerates their own
  actions, chance and everyone else are sampled once. Current strategies are
  refreshed lazily with a stamp per pass, regrets and averages live in flat
  arrays, and weighting (linear/DCFR discounts every `interval` iterations)
  works as in `Mccfr`. Three averaging rules: `Simple` (the two-player
  opponent-node rule), `Weighted` (divided by the sampled reach of the
  players who are neither traversing nor acting) and `Full` (an exact
  own-reach pass after each iteration). `accumulate` runs one averaging pass
  under a fixed profile with the regrets frozen, which is what the
  unbiasedness test needs.
- `Multi_br` rewritten over `Multi_tree`: one pass collects each responder
  infoset's histories and outside reach, then infosets are resolved deepest
  first (by the number of the responder's own decisions before them), each
  subtree evaluated with the best actions below it already fixed. It also
  exposes `values`, `gains` and `nash_conv` on a tree, so a solver's tree is
  reused instead of built again.
- `Multi_profile`: reach per infoset (total and own), a reach-weighted
  total-variation distance per player, and the own-reach mix of several
  profiles.
- CLI: `multi-es` (NashConv against nodes touched for full-tree CFR/DCFR and
  the sampled variants, mean over seeds, CSV and SVG) and `multi-select`
  (end points of CFR+, DCFR and several sampled seeds: NashConv, values,
  distances, mixes of the first k seeds). `leduc4` added to the game list.

Design notes:

- Why the two-player averaging rule is biased with three players: at a node
  of player p visited on player i's pass, the visit probability is pi_c times
  the reach of everyone but i. With two players that is pi_c pi_p, and pi_c
  summed over the infoset is constant, so normalizing per infoset removes it.
  With three, the third player's reach multiplies each iteration's
  contribution by something that changes from one iteration to the next.
  Dividing by it makes the estimate unbiased. It does not make it better in
  practice (below).
- Matching `Mccfr` bit for bit took the same tree order, the same stamp
  scheme for refreshing strategies, the same RNG draws in the same order, and
  the regret update written as v_a - v on the traverser's own payoff (equal to
  sign * (v_a - v) on player 0's payoff exactly, since negation is exact).
- `Full` averaging bumps the pass counter so the strategies it refreshes are
  not reused after a discount changes the regrets.
- Linear weighting in the sampler discounts every table entry, so doing it
  every iteration costs about 17,000 operations on an iteration that visits
  about 100 nodes. The comparison uses an interval of 1,000.

Results (one core each, several runs in parallel):

- Unbiasedness, three-player Kuhn with frozen regrets and two alternating
  profiles, largest error against the exact mix: simple 0.053, 0.052, 0.046,
  0.046 at 5k, 20k, 80k, 320k rounds; weighted 0.035, 0.010, 0.0064, 0.0025.
- Eight-card three-player Leduc, 3e9 nodes, 3 seeds: DCFR 7.0e-4, ES simple
  2.4e-2, weighted 2.7e-2, linear 2.8e-2, vanilla CFR 4.3e-2, full averaging
  1.65. ES is ahead of DCFR until about 2e7 nodes.
- Six-card three-player Leduc: every sampled variant except full averaging
  between 0.16 and 0.24 from 1e8 nodes on, vanilla CFR 0.28 to 0.45, DCFR
  9.2e-3.
- Four-player Leduc (25.7M nodes, 127,980 infosets), 1e10 nodes: ES 1.25e-2,
  weighted 1.32e-2, DCFR 5.4e-2 after 97 iterations. ES reaches DCFR's final
  number at about 7e8 nodes. 14M nodes/s for ES against 21M for DCFR.
- Best response: 1.17 s to 0.028 s on eight-card three-player Leduc, 204 s to
  12.4 s on four-player Leduc, NashConv equal to 12 digits.
- Which equilibrium (six-card, CFR+ and DCFR at 5,000 iterations, five seeds
  at 3e9 nodes): CFR+ and DCFR are 0.12/0.16/0.07 apart; the seeds are 0.025
  to 0.062 from each other, 0.03 to 0.08 from DCFR and 0.10 to 0.19 from
  CFR+. Mixing the first k seeds: NashConv 0.187, 0.154, 0.135, 0.120, 0.117.

Checks:

- 239 tests. New: the sampler against `Mccfr` on Kuhn and Leduc (three
  weightings, two averaging rules, exact equality); the averaging test above
  with thresholds 0.025 (weighted), 1e-12 (full) and 0.03 (simple must stay
  above it); only the traverser's strategy moves in a pass; seeds; every
  averaging rule near the Szafron family after 20,000 iterations. The new best
  response against the old one on random profiles (kuhn3, kuhn4, Leduc,
  leduc3-6). Distance and mix properties, including the mix's values being
  the mean.
- Charts rendered and looked at: docs/es-kuhn3.svg, docs/es-leduc3.svg,
  docs/es-leduc3-6.svg, docs/es-leduc4.svg.
- CI caught a race: the best response first kept its tree in a `lazy`, and
  the `abstract` command evaluates exploitability from two domains at once.
  Forcing a lazy value from two domains raises `CamlinternalLazy.Undefined`
  in OCaml 5. The tree is now built when the functor is applied; after that
  it is only read, and each call allocates its own scratch arrays.

Next: team play in three-player Kuhn (two players sharing a payoff against
the third), then a lazily expanded tree for n players so four-player Leduc
does not need 5 GB, then the gift-scaling experiment on the river re-solver.

## 2026-11-08: predictive CFR+, the discounting grid, dilated FTRL

Three newer ways to pick the next strategy, all measured against CFR+ and DCFR
on the same budget: predictive CFR+ and PDCFR+, a grid over DCFR's
(alpha, beta, gamma), and optimistic FTRL on the sequence form as a baseline
that is not CFR.

What was built:

- `Cfr.Predictive { alpha; gamma }`, with `pcfr_plus` (alpha infinite, gamma
  2) and `pdcfr_plus` (2.3, 5), plus `dcfr_plus` as a named DCFR setting
  (1.5, -inf, 4). `Cfr.Make` keeps the last pass's instantaneous regrets in an
  extra array and matches on stored + last (`Table.refresh_predicted`).
  `current_strategy` returns the predicted strategy, which is the one played.
- The same in `Subgame`: the regret store holds R d(t) + r and the solver
  keeps R and r per decision on the side, so buckets, suit folding, float32
  and pruning work unchanged. `regrets` subtracts r again.
- `Mccfr`, `Parallel`, `Multi_mccfr` and `Multi_cfr` reject a predictive
  algorithm instead of silently running without the prediction.
- `Dilated`: optimistic (or plain) FTRL with the dilated entropy regularizer.
  `Dilated.response` is the closed-form argmax, bottom-up over infosets. The
  first strategy is that argmax at zero, which is not uniform: an action with
  more infosets below it has more entropy behind it and gets more weight.
- CLI: `compare --algorithms LIST` and `--last` (current instead of average
  strategy), names `dcfr+`, `pcfr+`, `pdcfr+`, `oftrl:ETA`, `ftrl:ETA`;
  `subgame -a` takes the new names; `landscape` runs the grid on a domain
  pool and draws a heatmap per panel (`Plot.heatmap`).

Design notes:

- Why no second copy of R is needed: the existing discount multiplies stored
  regrets by d(t) at the end of each pass, so the store already holds
  d(t) R_t and [store + r_t]^+ is the paper's [R_t d(t) + r_t]^+. The
  reference implementation in the tests does not take this shortcut. It
  keeps R undiscounted and computes [R_{t-1} d(t-1) + r]^+ and
  [R_t d(t) + r]^+ literally, and the two agree to 1e-10 on random
  (alpha, gamma), with alpha infinite in the mix.
- Writing the reference turned up a bug in the reference itself: it computed
  t^-inf as 1 at t = 1 (that is what pow does), so beta = -inf did not mean
  clipping there. The library's `factor` already special-cased infinities.
- In the subgame solver the instantaneous regret is read back as
  store - R - r. With a float32 store that is a rounding error per entry per
  pass, the same order as what the float32 store already does to every
  regret update.
- Dilated entropy's argmax at an infoset is a softmax of eta times (action
  utility + values of the child infosets), and the value of an infoset with
  parent sequence p is log-sum-exp / eta, added to p. With per-infoset
  weights all equal to one this is the unweighted regularizer; Farina et al.
  need weights for their bounds.

Results:

- Kuhn, 10,000 iterations: CFR+ 9.6e-6, DCFR 2.4e-5, DCFR+ 1.0e-5, PCFR+
  2.4e-11, PDCFR+ 2.4e-16.
- Leduc, 100,000 iterations: CFR+ 4.7e-7, DCFR 1.1e-6, DCFR+ 5.6e-7, PCFR+
  2.4e-8, PDCFR+ 3.3e-12. At 3,000 iterations PDCFR+ is still eight times
  worse than DCFR; it crosses over before 26,000.
- Liar's dice at 3,000 iterations and the limit river at 20,000: DCFR and
  DCFR+ ahead, the predictive variants last (river: DCFR+ 6.7e-6% of the pot,
  PDCFR+ 6.1e-5%).
- Limit turn (4,330 decision nodes, 203 MB), 600 iterations, % of the pot:
  DCFR+ 4.6e-3, DCFR 4.9e-3, CFR+ 1.6e-2, PDCFR+ 1.6e-2, PCFR+ 1.8e-2, about
  four minutes per run.
- Last iterate on Leduc after 100,000 iterations: PCFR+ 1.7e-15, PDCFR+
  5.0e-16, CFR+ 6.9e-4, DCFR+ 1.2e-3, DCFR 3.0e-3.
- Dilated FTRL on Leduc, eta = 3: 4.9e-5 average after the work of 100,000
  CFR+ iterations; eta = 10, 30, 100 worse. On Kuhn the optimistic last
  iterate gets to 2.9e-14, plain FTRL stays at 0.25.
- Grid (229 settings each): on Leduc at 20,000 iterations the DCFR panels
  are flat for alpha >= 1.5, gamma >= 1 (within 5x of 1.3e-6), alpha = 0.5
  and gamma = 0 are bad everywhere, beta = 1 bottoms out at 3.2e-4, and the
  best PDCFR+ cell is (2, 6) at 2.0e-8 against 5.1e-8 for the paper's
  (2.3, 5). At 2,000 iterations the best PDCFR+ cell is six times behind the
  best DCFR cell instead. Liar's dice at 1,000: best DCFR 6.3e-5 at
  (2, 0, 2), PDCFR+ no better than 3.3e-4. Kuhn at 1,000: PDCFR+ with
  alpha >= 2 and gamma 6 between 8.6e-16 and 4.1e-15. The Liar's dice grid
  takes 940 s on six domains.

Checks:

- 260 tests. New: the predictive variants against the literal
  reference on Kuhn (named settings and 20 random ones); the predicted current
  strategy against the reference; PCFR+ and PDCFR+ against CFR+ on Kuhn;
  PDCFR+ behind DCFR at 1,000 iterations on Leduc and ten times ahead at
  30,000 (slow); the PCFR+ last iterate under 1e-12 on Kuhn while CFR+'s is
  above 1e-3; the subgame solver against the scalar solver for PCFR+ and
  PDCFR+ on the river, the turn, a bucketed turn, folded suits and pruning
  (1e-9); samplers rejecting prediction. For `Dilated`: the closed form
  against random strategies and perturbations of itself on a hand-built
  treeplex (300 cases), its value at zero, the first iterate on Kuhn, the
  average and the game value, optimism against plain FTRL on the last
  iterate.
- Mutants: dropping the prediction swap, matching without the prediction,
  writing back R instead of R + r in the subgame store and reading r back
  without subtracting the old r each fail four to seven tests; dropping the
  child values from the dilated softmax fails five, turning off optimism
  two.
- Every commit was built and tested on its own in a scratch worktree.
- Charts rendered and looked at: docs/leduc-modern.svg, docs/leduc-last.svg,
  docs/landscape-kuhn.svg, docs/landscape-leduc.svg,
  docs/landscape-liars.svg. The first heatmap shaded every panel on one
  scale, and Kuhn's 13 decades washed out the DCFR panels, so the shading is
  per panel and the numbers stay in the cells.
- zarith did not install (its conf-pkg-config dependency fails to build on
  this Windows switch), so the exact sequence-form LP in rationals is still
  open.

Next: the exact sequence-form LP once rationals are available (or a small
exact rational type), then team play in three-player Kuhn and the lazily
expanded n-player tree.

## 2026-11-08: exact equilibria from the sequence-form LP

The solvers measure their distance from equilibrium with a float best
response, and on Kuhn the best ones reach 1e-15, right where doubles run out.
This entry adds an exact solver to answer two questions: what is each game's
value as a fraction, and how far is a CFR strategy really from equilibrium
once rounding is out of the picture.

What was built:

- `Bigint`: arbitrary-precision integers with no dependency. Sign-magnitude
  with 30-bit limbs (14 bits on a 31-bit OCaml, so js_of_ocaml stays possible),
  schoolbook multiplication, Knuth's algorithm D for division, Euclid for gcd.
  Numbers below 2^60 live in a single machine word and only spill into limbs
  when an operation overflows. `Bigint.Make` takes the limb width, which is how
  the tests run everything again with 3-bit limbs: algorithm D's rare add-back
  step happens in roughly one division out of 2^29 with real limbs and all the
  time with tiny ones.
- `Rat`: fractions in lowest terms. Addition and multiplication cancel common
  factors first (Knuth 4.5.1), so the gcds stay small. `of_float` is exact
  (a double is a binary fraction); `recover` turns a chance probability such as
  0.1666... back into 1/6 by continued fractions.
- `Lp`: a dense tableau simplex, two-phase with artificials, written once as a
  functor over the number field and instantiated with rationals and with
  doubles. Dantzig's rule with a lexicographic ratio test on the rows of the
  basis inverse, and Bland's rule during long degenerate stalls. The float
  run comes first; its final basis is pivoted into the exact tableau (columns
  that cannot be pivoted in are skipped), and if that basis is primal feasible
  the exact simplex continues from it, otherwise it starts cold.
- `Sequence_form`: builds the realization-plan constraints and the sparse
  payoff matrix from any `Game.S` (checking perfect recall and that every
  chance node's recovered probabilities sum to exactly one), solves
  max_x min_y and max_y min_x as two LPs and fails if their values differ.
  It also converts any float strategy to exact realization plans and computes
  exact best responses by a backward pass over each player's treeplex.
- CLI: `cfr-solver exact -g GAME [--cold] [-n N --csv --svg]`. With `-n` it
  runs CFR, CFR+, DCFR, PCFR+ and PDCFR+ and prints float against exact
  exploitability at each checkpoint.

Design notes:

- Zarith would have been the obvious choice, but it does not install on this
  Windows switch (its pkg-config dependency fails). An own bignum also keeps
  C stubs out of the browser build later.
- The first version used Dantzig with a switch to Bland's rule for good after
  50 degenerate pivots. Leduc ran past 16,500 pivots in 100 s without
  finishing. Switching back to Dantzig after each nondegenerate pivot did not
  help (14,800 pivots, still going). The lexicographic ratio test was the fix
  for pivot count: Leduc with two ranks went from 834 + 876 pivots to
  376 + 342 cold.
- A pure float simplex cycled on these LPs until its ratio-test noise was
  cleaned (anything under 1e-9 is set to zero) and its pivot count capped at
  20 times rows plus columns. A tolerance-aware compare in the float ratio test
  made it worse (it never finished phase one), so ties are broken on raw
  doubles.
- The small-integer fast path made Leduc with two ranks six times faster
  (3.8 s to 0.6 s, before the pivot changes).

Results (all values checked against the published tables in the tests):

- Kuhn: -1/18, 16 + 16 pivots. Leduc with two ranks: exactly 0, 163 + 166
  pivots, 0.03 s.
- Leduc: -1454920850547486749701863871533/16995463438839962469905132501930
  = -0.085606424319, 358 + 362 pivots in 21 s. Cold, it did not finish in 15
  minutes.
- Liar's dice: 1x2 0 and 1/2 (wild), 1x3 -1/9 and 1/9, 1x4 -1/8 and 1/16,
  2x2 1/8 and 7/8, the same fractions as the published LP values the existing
  tests quote. All but 1x4 wild solve in under half a second. On 1x4 wild the
  float pass takes 49,477 pivots and ends on a basis with 126 negative rows in
  exact arithmetic, so the exact solve starts cold: 2538 + 909 pivots, 52 s.
- On every other game the float basis was already exactly optimal: the exact
  simplex made no pivots beyond installing it.
- True gap on Kuhn (docs/exact-kuhn.svg): float and exact best responses agree
  to three digits down to about 1e-13. PDCFR+ at 10,000 iterations is 2.36e-16
  by the float best response and 1.88e-16 exactly. From 5,000 to a million
  iterations PDCFR+ wanders between 5.3e-17 and 7.5e-16 exactly, and PCFR+
  flattens between 7.5e-16 and 2.9e-15 after 200,000. The floor belongs to the
  float-averaged strategy, not to the best response measuring it.

Checks:

- 289 tests (full suite 218 s). New: bigint against native ints, the division
  identity and remainder bound for 60-digit numbers with 30-bit and 3-bit
  limbs, the two limb widths agreeing on products, quotients and gcds, gcd
  properties, 30! and 2^100. Rationals: field laws and lowest terms against
  the naive formulas, float round trips over the whole exponent range
  (subnormals included), continued-fraction recovery. Simplex: Wyndor, a
  phase-one problem, equality and redundant rows, negative right-hand sides,
  Beale's cycling example under Bland, Dantzig and the warm start,
  infeasible and unbounded, and 300 random 2D LPs against vertex enumeration
  in exact arithmetic, warm and cold. Sequence form: Kuhn sizes, -1/18 and an
  LP equilibrium inside the alpha family, exact best responses equal to the
  float ones on random strategies (Kuhn and Leduc with two ranks), weights
  normalized exactly, CFR+'s value within its NashConv of the LP value, the
  Liar's dice fractions, warm and cold agreeing, Leduc's -0.085606 (slow),
  1x4 wild (slow), and rejection of imperfect recall and bad chance nodes.
- Mutants: algorithm D without the add-back (4 failures) or without the qhat
  correction (6), the Knuth addition shortcut dividing the wrong denominator
  (2), the wrong sign on child infosets in the dual constraints (5), min
  instead of max in the best response (7). Removing the lexicographic tie-break
  or the primal check after the warm start fails nothing: both affect speed or
  guard a rare float failure, and the final answer is certified by the exact
  NashConv anyway.
- Every commit was built and its new tests run on its own.
- Chart rendered and looked at: docs/exact-kuhn.svg.

Next: team play in three-player Kuhn and the lazily expanded n-player tree.
The exact solver opens two follow-ups: a sparse LU in place of the dense
tableau (Leduc's 21 s has not been profiled yet, but its exact entries reach
30-digit fractions), and
exact values for the turn and river subgames once they fit.

## 2026-11-08: sealed-bid auctions, a market-making card game

Two games outside poker, both about trading.

Auctions (`lib/auction.ml`, `auction`, `auction-bids`, `auction-reserve`):

- First-price, second-price and all-pay sealed-bid auctions with n bidders,
  values and bids on the integers 0..x, uniform independent values, split or
  void ties, and a reserve below which a bid is a pass. Void ties follow
  Rasooly and Gavidia-Calderon ("The importance of being discrete"), which
  gave the exact theorems to replicate.
- The payoff of bid b for value v has a closed form. Multiply out
  P(rival < b) + P(rival = b) z over the rivals: coefficient k is the chance
  that exactly k rivals tie at b and the rest are below. The share won is
  sum c_k / (k + 1) (split) or c_0 (void), and every rule is
  u(v, b) = v W(b) - pay(b). Second price accumulates
  P(max rival = m) max(r, m) as b sweeps up. O(x n^2) per bidder.
- The evaluator is a functor over a field: floats for the solver, `Rat` for
  exact certificates (pure-strategy searches, last iterates rounded to small
  fractions or taken exactly from their doubles).
- The solver is regret matching per (bidder, value), with the same update
  order, discounting and averaging as `Multi_cfr`; it matches the full-tree
  solver iterate for iterate. `--symmetric` shares one strategy between all
  bidders.
- Theory: (n - 1)/n v for first price, v - (v^n - r^n)/(n v^(n-1)) with a
  reserve, (n - 1)/n v^n for all-pay, and revenue
  2n/(n + 1)(1 - r^(n+1)) - (1 - r^n), which peaks at r = 1/2 (Myerson).

Results:

- Exact: truthful second price is an equilibrium for every tie rule and
  reserve tried; first price with two bidders and void ties has exactly the
  symmetric pure equilibria floor(v/2) and ceil(v/2) for x = 2..7; three
  bidders have none for x = 4, 5, 6.
- Symmetric CFR+ on three bidders, x = 12, void ties finds the mixed symmetric
  equilibrium: multiples of 3 bid 2v/3, value 4 bids 2 with probability
  3 sqrt 2 - 4 (from 2 (3/13)^2 = ((4 + p)/13)^2), exact NashConv of the
  iterate 9e-16. Without sharing, CFR+ ends on asymmetric equilibria whose
  last iterate has exact NashConv 0.
- Split ties with two bidders do not converge: x = 10, CFR+ and DCFR averages
  sit near 1.6e-4 of the top value from 10k to 100k iterations, LCFR and
  vanilla near 5e-4. Expected for general-sum no-regret dynamics.
- x = 100, 5,000 iterations: revenue 0.3326, 0.4996, 0.6669 against 1/3, 1/2,
  2/3 for first price with 2, 3, 5 bidders; all-pay 0.3309 and 0.6655. Bid
  error above a quarter of the top value at most 0.028 of it. Low values with
  five bidders barely ever win and stay near their starting mix.
- Reserve sweep on x = 40: best reserve 0.5 for both rules with two bidders
  and second price with three, 0.45 for first price with three (flat peak).
- Speed: 5,000 iterations at x = 100 went from 6.9 s to 1.55 s (two
  bidders) and 41.6 s to 5.9 s (five) by moving marginals out of the boxed
  functor, caching current strategies and replacing a float fold in
  `Regret_matching.strategy_into` with a loop. Output bit-identical; the full
  suite passes with the regret-matching change.

Market-making game (`lib/market.ml`, `market`, `market-width`):

- Deck 1..k, one card each to maker and taker, optional hidden cards; the sum
  settles. The maker quotes mid m at fixed width w, the taker buys, sells or
  passes. Optional second round where the taker quotes back. Zero-sum, so
  CFR, best response and the exact LP all apply.
- Deck of 3, width 0: quoting fair value loses 2/3 (by hand), the LP
  equilibrium loses 1/3. Deck of 8 with a hidden card, width 3: equilibrium
  -0.3122, revealing -0.4821, uninformed -0.3571, and the equilibrium pools on
  quotes 13 and 14 with crossed assignments (a 2 quotes 14, a 7 quotes 13).
- Deck of 10 with a hidden card, widths 0..10 by DCFR (2,000 iterations,
  NashConv under 1e-5): the equilibrium breaks even from width 7, uninformed
  from 8, revealing from 9.
- Two rounds, deck of 4, width 1: the first quoter is +1/4 exactly (110 s in
  the LP), because the second round lets them trade on what the first trade
  showed. A deck of 5 did not finish in ten minutes in the dense LP.
- Exact LP: fine up to six cards, but eight cards with a hidden card ran for
  more than nine minutes at width 3.

Checks:

- 20 new tests. Auctions: tree values and gains for 24 configurations,
  iterate-for-iterate equality with `Multi_cfr` (four algorithms), the exact
  theorems above, E[min of two] revenue, continuous formulas against a
  Simpson integral, convergence bounds, exact zero NashConv of last iterates,
  the 3 sqrt 2 - 4 mix. Market: terminal and infoset counts, payoffs along
  paths, the 2/3 by hand, zero value for wide markets, LP inside the DCFR
  bracket with exact gap 0, equilibrium beating both naive quotes.
- Mutants: second price without the reserve floor (caught), split ties
  dividing by k instead of k + 1 (caught), the sell price on the wrong side of
  mid (caught), the maker's sign flipped (caught).
- Charts rendered and looked at: docs/auction-bids.svg,
  docs/auction-reserve.svg, docs/market-width.svg.

Next: a sparse LU for the exact LP (the market game and Leduc are both
limited by the dense tableau), then team play in three-player Kuhn and
Figgie's rules engine.

## 2026-11-08: Figgie rules engine and bots

Built:

- `lib/figgie.ml`: the 12 decks, goal suit, deal, settlement with split
  bonuses, an exact posterior over decks from a dealt hand (optionally
  conditioned on lower bounds for each suit), expected payout over the
  hypergeometric split of the remaining goal cards among the other hands, and
  marginal buy and sell values. The market keeps one best bid and offer per
  suit, rejects quotes that do not improve, trades crossing quotes at the
  resting price, and clears every quote after a trade. `play` runs a game in
  ticks with a random order each tick.
- `lib/figgie_bots.ml`: passive, zero-intelligence noise (random limit
  price), fundamentalist, card-counting counter, bottom-feeder, and a
  tournament that shuffles seats and reports mean profit and standard error.
- `figgie`, `figgie-hand` and `figgie-edge` commands, docs/figgie-edge.svg.

Design notes:

- Buy values condition on the card existing outside the hand. The first
  version did not, so with ten spades an eleventh came out at -1.96 (decks
  where it cannot exist scored 0). Now the posterior is restricted to decks
  consistent with the hand after the trade, and an impossible hand raises.
- Card counting keeps, for each player, how many cards of each suit are known
  to be there. A sale by a player with none known proves a new card, so the
  seen counts are a lower bound on the deck that the posterior conditions on.
  It ignores the softer evidence in which suits people choose to trade.
- The bonus odds depend only on (cards held, goal cards outside, sorted other
  hand sizes), so they are memoized in a per-domain table keyed by one int.
  3,000 five-player games went from 60.7 s to 13.6 s with identical output.
- The first noise trader lifted offers at any price, which made the counter's
  profit grow without bound in its quote edge (+93 at edge 30). It now trades
  only inside a random limit price, and the curve flattens near +31 by edge 20.

Results (3,000 games unless noted):

- fundamentalist, counter, bottom-feeder, noise: +15.15, +17.90, +14.21,
  -47.26 (standard errors about 1).
- Two counters against two fundamentalists: +1.34, +0.81 against -0.77,
  -1.38.
- Three counters and a passive player: the passive one loses 20.69 a game.
- Edge sweep (1,500 games a point): counter +5.81 at edge 0, +31.28 at 20;
  the fundamentalist falls from +22.94 to +3.42 as the counter stops giving it
  quotes to pick off.

Checks:

- 14 new tests: deck structure, 198/204 for ten spades, uniform posterior for
  an empty hand, seen bounds, posterior calibration against 40,000 deals,
  the tie split from the official rules page, pot fully paid, a locked bonus
  giving exact buy and sell values of 10 and 0, expected payout against
  50,000 simulated deals, the average expected payout equal to the ante for
  four and five players, order book rules step by step, conservation over 40
  random games, the counter's lower bound over 40 random games, and the
  fundamentalist beating three noise traders.
- Mutants: three-way tie split (two tests fail), every sale counted as new
  (tally bound fails).

Next: an abstracted Figgie for n-player MCCFR (card-count beliefs, a few price
levels), scored against these bots, then Goofspiel against published numbers.

## 2026-11-08: Lemke's algorithm, general-sum auctions

CFR has no convergence guarantee off zero-sum games, and the two-bidder
auction with split ties showed it: the averages sat near 1.6e-4 of the top
value from 10k to 100k iterations. The LP from the exact solver only handles
zero-sum games. This entry adds an exact solver for any two-player game with
perfect recall and uses it on auctions that are not symmetric.

What was built:

- `Lemke`: builds the sequence form of a two-player `Multi.S` game (both
  payoff matrices, plus the chance reach of each sequence pair so payoffs can
  be shifted), then runs Lemke's algorithm on the LCP of Koller, Megiddo and
  von Stengel with the covering vector of von Stengel, van den Elzen and
  Talman: the prior's payoffs on the best-response rows and e, f on the
  realization rows. At z0 = 1 the only solution is x = y = 0, so the start
  basis is built directly: z0, all duals, the sequences chosen by a full best
  response to the prior (one per infoset, even unreached ones, basic at zero)
  and the slacks of the rest. x for the empty sequence and its slack are both
  nonbasic, and x for the empty sequence enters first.
- The tableau is a dense `Rat` matrix with an identity block appended once the
  start basis is installed. That block becomes B^-1 B0, and the ratio test
  breaks ties lexicographically on it, which is the same as perturbing the
  right-hand side by (eps, eps^2, ...) in the start basis coordinates. Every
  start row is then lexicographically positive, even though most sequences
  are basic at zero. Free duals never enter the ratio test.
- Priors are rounded to whole percents before use. A float prior converted
  exactly gave 53-bit denominators in every gradient entry and random priors
  took 30 s on a 50-sequence auction; rounded, 0.07 s. The prior only chooses
  the path, so this changes which equilibrium is found, never whether it is
  one.
- `Lemke.gains` is the certificate: an exact backward pass over each
  treeplex for the best response, minus the value. Every result in this entry
  has both gains equal to 0.
- `Bidding`: two-bidder first-price, second-price and all-pay auctions with
  any joint prior over integer signals and any value function. Constructors
  for independent uniform signals on different ranges, an affiliated prior
  (density proportional to x + s0 s1, log-supermodular because
  f(a', b') f(a, b) - f(a', b) f(a, b') = x (a' - a)(b' - b)), private values
  and the wallet game's common value s0 + s1.
- CLI: `exact -g GAME --lemke`, `auction-lemke [--top --void --priors K]`,
  `bidding --case asymmetric|affiliated|wallet --top X`.

Results:

- Zero-sum games, Lemke against the LP: Kuhn 13 pivots against 16 + 16,
  two-rank Leduc 167 against 163 + 166, Liar's dice 1x3 37 against 187 + 180,
  2x2 143 against 775 + 739, 1x4 87 against 805 + 809. Same fractions every
  time. Lemke is slower in wall time (4.82 s against 0.03 s on two-rank Leduc)
  because the LP starts from a float basis and Lemke pivots a dense rational
  tableau of about 440 by 1,200 entries.
- First price, split ties, x = 10: 9 pivots, 0.6 s. Symmetric, pure except at
  v = 7 (3 with probability 8/19) and v = 10 (4 with probability 9/19).
  Revenue 138707/43681. Three random priors found the same equilibrium.
- Void ties, x = 8: the uniform prior and six random ones gave four distinct
  equilibria, with revenues 176/81, 8/3 (twice) and 212/81.
- All-pay, x = 8: pure, bids 0 up to v = 3, revenue 20/9, 45 to 49 pivots from
  every prior.
- Asymmetric, values 0..8 against 0..4: the weak bidder mixes 1 and 2 at
  v = 3 (mean 1.667) where the strong bids 1. First price 233/135 = 1.726,
  second price 14/9 = 1.556. x = 12: 2.681 against 2.385.
- Affiliated, x = 8: second price 3.883, first price 3.558 (mixes with
  denominators near 371,347). x = 10: 4.927 against 4.591.
- Wallet, x = 8, bids up to 16: first price 157 pivots, 2.5 s, bids close to
  the continuous b(s) = s (1 1 1 3 3 5 5 7 7 for s = 0..8), revenue 401/81
  against 136/27 for second price with b(s) = 2s. A first-price bidder who
  bids E[V | s] against the equilibrium loses 0.358 a round; in second price
  the same naive bid earns 1.395 against 1.481.

Checks:

- 14 new tests (full suite 337 tests, 197 s). Lemke: Kuhn and two-rank Leduc
  values, a 2x2 game whose only equilibrium is mixed (2/3, 1/4), Battle of the
  Sexes steered to each pure equilibrium by the prior, the x = 10 mix, zero
  NashConv from `Auction.Exact` (written separately, closed-form payoffs) for
  void and split ties on x = 2..6, and a qcheck property over 60 random
  general-sum trees with chance, hidden moves and perfect recall by
  construction: valid plans, gains exactly 0. Bidding: the Lemke equilibrium
  of the symmetric case has zero NashConv in `Auction.Exact`, values from the
  tree match a direct sum over the prior, truthful second price with
  asymmetric priors, 2s and not the naive bid in the wallet game, and the
  affiliation inequality at every pair of grid points.
- Mutants: flipping the sign of the child-infoset duals fails three tests
  (Kuhn, Leduc, the random games). Dropping the preference for z0 in the ratio
  test fails nothing, which is expected: the lexicographic rule already picks
  a unique row and the preference only ends the path one pivot earlier.
- A wrong expectation in the first Battle of the Sexes test: from the uniform
  prior both players switch at the same point (t = 1/3) and the path ends at a
  pure equilibrium, not the mixed one. The test now only asks for a pure one.

Next: integer pivoting (entries kept as integers over a common determinant,
as in Avis's lrs) to cut the rational overhead, then Goofspiel
and the abstracted Figgie for n-player MCCFR.

## 2026-11-09: exploiting opponents, PSRO

Everything so far computed equilibria. This entry is about beating a specific
opponent without becoming easy to beat, and about the population-based view of
equilibrium finding.

What was built:

- `Model`: counts of the opponent's actions per information set from observed
  hands (full information afterwards, the setting of Johanson and Bowling 2009),
  turned into a strategy with an optional Dirichlet prior. Unseen sets are
  uniform.
- `Bots`: fixed opponents built from the tree (uniform, first matching label
  such as "always raise", a behavioural blend of two strategies) and `part`,
  which cuts one player's half out of a profile.
- `Restricted.game`: one functor-free construction that wraps any `Game.S`.
  With `p` it is the restricted Nash response (Johanson, Zinkevich and Bowling
  2007): a root coin pins the opponent to the model (their decisions become
  chance nodes) or leaves them free. With `conf` it is the data-biased response:
  the coin sits at every opponent information set, with probability P_conf(I)
  from the 1-step, 10-step, 0-10 linear or s-curve functions of the paper, and
  0 for sets never seen. The result is an ordinary game, so `Cfr`, the best
  response and the exact LP work on it unchanged.
- `Restricted.evaluator`: exact value against an opponent, worst case and best
  response, on a prebuilt `Multi_tree`.
- `Safe.session`: repeated hands against an opponent that may depend on our
  current strategy, the model refitted every k hands, policies equilibrium,
  best response, RNR, DBR and two "risk what you've won" budgets (Ganzfried and
  Sandholm 2015). `Safe.adversary` baits, then best-responds exactly to
  whatever we play on each hand.
- `Psro`: populations of pure best responses, meta-game solved exactly
  (rational simplex), with the float simplex, or uniformly. `Psro.mixture`
  combines strategies by reach weight, which is what "play strategy k with
  probability w_k" means in behavioural terms. `Lp.maximize_float` exposes the
  float simplex that used to be only the warm start.
- CLI: `frontier`, `safety`, `psro`.

Design notes:

- The budget's safety argument: let v be the worst case of our equilibrium and
  loss(s) = v - worst(s). The policy only plays s with loss(s) <= k. With the
  expected update k' = k + u(s, t) - v >= k - loss(s) >= 0 for every opponent
  t, so k never goes negative and the session's expected total never drops
  below the game value at any point. With realized payoffs k also grows on card
  luck, and the policy then gambles winnings that were never gifts.
- Defining v as our own equilibrium's guaranteed value (not the true game
  value) makes the equilibrium's loss exactly 0, so the fallback always fits
  the budget even though CFR's equilibrium is approximate.
- The DBR coin at every opponent node breaks perfect recall for an opponent
  who acts twice in a hand (their later key no longer tells whether the earlier
  move was free). CFR still computes the right thing, since the free part of
  the strategy only scales each set's regrets by (1 - c), but the sequence-form
  LP cannot take that game. The tests only send the LP games where the opponent
  moves once.
- First bug found: `against` took the union of our strategy and the
  opponent's, and the union keeps the left side, so passing a full equilibrium
  profile as "our strategy" silently replaced the opponent with our own second
  seat. The evaluator now cuts our half out first.

Results:

- Leduc, opponent half equilibrium and half "always raise", model from 1000
  probe hands (120 of 144 opponent sets seen), 2000 CFR+ iterations per point
  (docs/exploit-leduc.svg). With the true strategy, RNR at p = 0.9 gains 1.200
  of the 1.214 a best response gains at worst-case loss 0.546 instead of 1.414.
  With the model, RNR peaks at p = 0.5 (0.849 for 0.280) and decays to 0.607
  gain and 2.849 loss at p = 1. DBR with 0-10 linear: 1.076 for 0.652 at
  Pmax = 0.9. Same shape as the 2009 paper.
- Safety, Kuhn, 20 seeds, bait 300 hands then exact best responses: best
  response ends -401 below 3000 v, RNR p = 0.9 -43, realized budget -35 with 12
  of 20 seeds below 0, expected budget exactly 0.00 with none below.
  Equilibrium +56. With the bait kept all session, expected budget +1581,
  equilibrium +556, best response +1581.
- Safety, Leduc, 10 seeds: best response -3984, RNR p = 0.9 -1474, DBR -301,
  realized budget -125 (8 seeds below 0), expected budget +0.02 (none),
  equilibrium +203, RNR p = 0.5 +64 (one seed dipped to -12.5).
- PSRO on Kuhn: 8 iterations to value -1/18 and NashConv 0 with either
  meta-solver; uniform meta still 0.108 after 50. Leduc: double oracle NashConv
  4.4 at 10 iterations, 0.66 at 60, 0.35 at 100, 0.122 at 160 (124 s);
  uniform 0.731 at 160 (2.9 s). The meta LP is solved from scratch every
  iteration and grows with both populations; I have not profiled the split.

Checks:

- 16 new tests (full suite 353). The restricted game's CFR+ value against the
  exact LP for four (model, p) pairs; DBR with constant c against the LP of RNR
  with p = c; RNR beats the equilibrium, the best response and every other p's
  solution on its own objective; the frontier is monotone; endpoints are Nash
  and a best response; DBR does not trust unseen sets (worst case -0.111
  against -1/6 for RNR with a uniform default). Restricted games pass the
  game checker (probabilities, labels, perfect recall). The budget never goes
  below 0 on three seeds; best response drops below -300. PSRO: exact matrix
  games, mixture linearity on Leduc to 1e-12, double oracle on Kuhn.
- Mutants: swapping c and 1 - c in the DBR coin fails one test, swapping p and
  1 - p at the root fails five.

Next: warm-started meta LPs (or a few iterations of the meta-game solver
carried over) so double oracle on Leduc can run to convergence, PSRO with
approximate best responses from CFR as the oracle, then Goofspiel against
published numbers.
