open Cfr_solver
module E = Eval.Make (Kuhn)
module B = Best_response.Make (Kuhn)

let exact = Alcotest.float 1e-12

let of_bets player bets =
  List.fold_left2
    (fun s (key, actions) p ->
      Strategy.add key ~actions ~probs:[| 1. -. p; p |] s)
    Strategy.empty (E.infosets player) bets

let first_action =
  Strategy.union
    (of_bets P0 (List.init 6 (Fun.const 0.)))
    (of_bets P1 (List.init 6 (Fun.const 0.)))

(* Reference values from OpenSpiel's exploitability_test.py. *)
let openspiel_nash_conv () =
  Alcotest.check exact "uniform" (11. /. 12.) (B.nash_conv Strategy.empty);
  Alcotest.check exact "first action" 2. (B.nash_conv first_action);
  Alcotest.check exact "exploitability is half" (11. /. 24.)
    (B.exploitability Strategy.empty)

let equilibrium_is_unexploitable () =
  List.iter
    (fun alpha ->
      let s = Kuhn.equilibrium alpha in
      Alcotest.check exact "player 0" (-1. /. 18.) (B.value P0 s);
      Alcotest.check exact "player 1" (1. /. 18.) (B.value P1 s);
      Alcotest.check exact "nash conv" 0. (B.nash_conv s))
    [ 0.; 0.2; 1. /. 3. ]

let actions player sigma =
  let br = B.strategy player sigma in
  List.map
    (fun (key, _) ->
      match Strategy.find br key with
      | Some { actions; probs } ->
          (key, actions.(if probs.(0) = 1. then 0 else 1))
      | None -> Alcotest.failf "no entry for %s" key)
    (E.infosets player)

(* OpenSpiel's best-response actions against the uniform policy, including
   the exact tie for player 0 holding a king (bet and check are both worth
   1.5) that goes to the lower action. *)
let openspiel_actions () =
  let pairs = Alcotest.(list (pair string string)) in
  Alcotest.check pairs "player 0"
    [
      ("J", "b");
      ("Jpb", "p");
      ("Q", "b");
      ("Qpb", "b");
      ("K", "p");
      ("Kpb", "b");
    ]
    (actions P0 Strategy.empty);
  Alcotest.check pairs "player 1"
    [
      ("Qp", "b");
      ("Qb", "b");
      ("Kp", "b");
      ("Kb", "b");
      ("Jp", "b");
      ("Jb", "p");
    ]
    (actions P1 Strategy.empty)

(* Every pure strategy of a player, built infoset by infoset. Kuhn has 2^6 per
   player, few enough to enumerate. *)
let rec pure = function
  | [] -> [ Strategy.empty ]
  | (key, actions) :: rest ->
      let n = Array.length actions in
      List.concat_map
        (fun s ->
          List.init n (fun a ->
              let probs = Array.init n (fun b -> if a = b then 1. else 0.) in
              Strategy.add key ~actions ~probs s))
        (pure rest)

let brute_force player sigma =
  List.fold_left
    (fun best s ->
      Float.max best
        (Game.sign player *. E.expected_value (Strategy.union s sigma)))
    Float.neg_infinity
    (pure (E.infosets player))

let profile =
  QCheck2.Gen.(
    map2
      (fun b0 b1 -> Strategy.union (of_bets P0 b0) (of_bets P1 b1))
      (list_size (return 6) (float_bound_inclusive 1.))
      (list_size (return 6) (float_bound_inclusive 1.)))

(* The best response must match the best of all 64 pure strategies, and
   playing the returned strategy must actually earn that value. *)
let matches_brute_force sigma =
  List.for_all
    (fun player ->
      let v = B.value player sigma in
      let earned =
        Game.sign player
        *. E.expected_value (Strategy.union (B.strategy player sigma) sigma)
      in
      Float.abs (v -. brute_force player sigma) < 1e-12
      && Float.abs (v -. earned) < 1e-12)
    [ Game.P0; P1 ]

let nash_conv_nonnegative sigma = B.nash_conv sigma >= -1e-12

module C = Cfr.Make (Kuhn)

let cfr_exploitability () =
  let t = C.create () in
  let at n =
    C.run t (n - C.iterations t);
    B.exploitability (C.average_strategy t)
  in
  let e100 = at 100 in
  let e1k = at 1_000 in
  let e10k = at 10_000 in
  if not (e100 > e1k && e1k > e10k) then
    Alcotest.failf "not decreasing: %g %g %g" e100 e1k e10k;
  if e10k > 3e-3 then Alcotest.failf "exploitability %g after 10k" e10k

let qcheck name prop =
  Alcotest.test_case name `Quick (fun () ->
      QCheck2.Test.check_exn (QCheck2.Test.make ~count:300 ~name profile prop))

let tests =
  [
    Alcotest.test_case "OpenSpiel nash conv" `Quick openspiel_nash_conv;
    Alcotest.test_case "equilibrium" `Quick equilibrium_is_unexploitable;
    Alcotest.test_case "OpenSpiel best-response actions" `Quick
      openspiel_actions;
    qcheck "matches brute force" matches_brute_force;
    qcheck "nash conv >= 0" nash_conv_nonnegative;
    Alcotest.test_case "cfr exploitability falls" `Quick cfr_exploitability;
  ]
