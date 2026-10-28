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
