open Cfr_solver

let probs = Alcotest.(array (float 1e-12))

let positive_part () =
  Alcotest.check probs "proportional"
    [| 2. /. 3.; 0.; 1. /. 3. |]
    (Regret_matching.strategy [| 2.; -1.; 1. |])

let no_positive_regret () =
  Alcotest.check probs "uniform"
    [| 0.25; 0.25; 0.25; 0.25 |]
    (Regret_matching.strategy [| 0.; -3.; -1.; 0. |])

let normalize () =
  Alcotest.check probs "scaled" [| 0.25; 0.75 |]
    (Regret_matching.normalize [| 1.; 3. |]);
  Alcotest.check probs "all zero" [| 0.5; 0.5 |]
    (Regret_matching.normalize [| 0.; 0. |])

(* Rock, paper, scissors, payoffs to the row player. *)
let rps = [| [| 0.; -1.; 1. |]; [| 1.; 0.; -1. |]; [| -1.; 1.; 0. |] |]
let dot x y = Array.fold_left ( +. ) 0. (Array.map2 ( *. ) x y)
let utilities opp = Array.map (fun row -> dot row opp) rps

let update regrets sigma u =
  let ev = dot sigma u in
  Array.iteri (fun a ua -> regrets.(a) <- regrets.(a) +. ua -. ev) u

let accumulate sums sigma =
  Array.iteri (fun a p -> sums.(a) <- sums.(a) +. p) sigma

(* Against a fixed rock-heavy opponent the average strategy goes to paper. *)
let exploits_fixed_opponent () =
  let opp = [| 0.4; 0.3; 0.3 |] in
  let regrets = Array.make 3 0. and sums = Array.make 3 0. in
  for _ = 1 to 10_000 do
    let sigma = Regret_matching.strategy regrets in
    accumulate sums sigma;
    update regrets sigma (utilities opp)
  done;
  let avg = Regret_matching.normalize sums in
  Alcotest.(check bool) "mostly paper" true (avg.(1) > 0.99)

(* In self-play the average strategies converge to the unique equilibrium,
   uniform play, even from lopsided starting regrets. RPS is symmetric, so the
   same payoff matrix serves both players. *)
let self_play () =
  let r0 = [| 5.; 0.; 0. |] and r1 = [| 0.; 0.; 3. |] in
  let s0 = Array.make 3 0. and s1 = Array.make 3 0. in
  for _ = 1 to 100_000 do
    let sigma0 = Regret_matching.strategy r0 in
    let sigma1 = Regret_matching.strategy r1 in
    accumulate s0 sigma0;
    accumulate s1 sigma1;
    update r0 sigma0 (utilities sigma1);
    update r1 sigma1 (utilities sigma0)
  done;
  let third = Alcotest.(array (float 0.01)) in
  let uniform = [| 1. /. 3.; 1. /. 3.; 1. /. 3. |] in
  Alcotest.check third "player 0" uniform (Regret_matching.normalize s0);
  Alcotest.check third "player 1" uniform (Regret_matching.normalize s1)

let tests =
  [
    Alcotest.test_case "positive part" `Quick positive_part;
    Alcotest.test_case "no positive regret" `Quick no_positive_regret;
    Alcotest.test_case "normalize" `Quick normalize;
    Alcotest.test_case "exploits a fixed opponent" `Quick
      exploits_fixed_opponent;
    Alcotest.test_case "rps self-play" `Quick self_play;
  ]
