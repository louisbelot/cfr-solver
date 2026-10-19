open Cfr_solver
module C = Cfr.Make (Kuhn)
module E = Eval.Make (Kuhn)

let solve n =
  let t = C.create () in
  C.run t n;
  t

let bet s key = (Strategy.probs s key ~n:2).(1)

let close tol name expected actual =
  if Float.abs (expected -. actual) > tol then
    Alcotest.failf "%s: expected %.4f, got %.4f (tolerance %g)" name expected
      actual tol

let first_iteration_is_uniform () =
  let t = solve 1 in
  Alcotest.(check int)
    "every infoset" 12
    (Strategy.cardinal (C.average_strategy t));
  List.iter
    (fun (key, { Strategy.probs; _ }) ->
      Alcotest.(check (array (float 0.))) key [| 0.5; 0.5 |] probs)
    (Strategy.bindings (C.average_strategy t))

(* From OpenSpiel's cfr_test.py (simultaneous updates, uniform averaging).
   Player 1 reaches "Qb" with own probability 1. Iteration 1 plays [1/2, 1/2]
   there and iteration 2 plays [0, 1], so the cumulative strategy is
   [1/2, 3/2]. *)
let openspiel_two_iterations () =
  let s = C.average_strategy (solve 2) in
  Alcotest.(check (array (float 1e-12)))
    "Qb" [| 0.25; 0.75 |]
    (Strategy.probs s "Qb" ~n:2)

let game_value () =
  let s = C.average_strategy (solve 10_000) in
  close 1e-4 "value" (-1. /. 18.) (E.expected_value s)

(* The equilibrium is a one-parameter family, so CFR may land anywhere in it.
   Player 0's probabilities are tied to its own alpha (the jack bluff rate);
   player 1's are unique. *)
let lands_in_equilibrium_family () =
  let s = C.average_strategy (solve 10_000) in
  let alpha = bet s "J" in
  if alpha < 0. || alpha > 1. /. 3. then Alcotest.failf "alpha = %g" alpha;
  close 0.03 "K bets 3 alpha" (3. *. alpha) (bet s "K");
  close 0.02 "Q calls alpha + 1/3" (alpha +. (1. /. 3.)) (bet s "Qpb");
  close 0.01 "Q checks" 0. (bet s "Q");
  close 0.01 "J folds" 0. (bet s "Jpb");
  close 0.01 "K calls" 1. (bet s "Kpb");
  close 0.01 "player 1 bluffs J" (1. /. 3.) (bet s "Jp");
  close 0.01 "player 1 checks Q" 0. (bet s "Qp");
  close 0.01 "player 1 bets K" 1. (bet s "Kp");
  close 0.01 "player 1 folds J" 0. (bet s "Jb");
  close 0.01 "player 1 calls Q" (1. /. 3.) (bet s "Qb");
  close 0.01 "player 1 calls K" 1. (bet s "Kb")

let tests =
  [
    Alcotest.test_case "first iteration is uniform" `Quick
      first_iteration_is_uniform;
    Alcotest.test_case "OpenSpiel two iterations" `Quick
      openspiel_two_iterations;
    Alcotest.test_case "game value -1/18" `Quick game_value;
    Alcotest.test_case "equilibrium family" `Quick lands_in_equilibrium_family;
  ]
