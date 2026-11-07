open Cfr_solver
module P = Play.Make (Kuhn)
module E = Eval.Make (Kuhn)

let follow rng sigma infoset labels =
  let probs = Strategy.probs sigma infoset ~n:(List.length labels) in
  List.nth labels (Rng.draw rng probs)

let mean_payoff seat hands =
  let rng = Rng.make 11 and own = Rng.make 12 in
  let sigma = Kuhn.equilibrium 0.2 in
  let total = ref 0. in
  for _ = 1 to hands do
    let r = P.hand ~rng ~seat ~bot:sigma ~ask:(follow own sigma) in
    total := !total +. r.payoff
  done;
  !total /. float hands

let equilibrium_value () =
  let hands = 200_000 in
  let tol = 4. *. 1.3 /. sqrt (float hands) in
  Alcotest.(check (float tol)) "player 0" (-1. /. 18.) (mean_payoff P0 hands);
  Alcotest.(check (float tol)) "player 1" (1. /. 18.) (mean_payoff P1 hands)

let seat_sees_own_infosets () =
  let rng = Rng.make 3 in
  let own = List.map fst (E.infosets P1) in
  for _ = 1 to 200 do
    let r =
      P.hand ~rng ~seat:P1 ~bot:Strategy.empty ~ask:(fun infoset labels ->
          if not (List.mem infoset own) then
            Alcotest.failf "asked at %s" infoset;
          Alcotest.(check (list string)) "labels" [ "p"; "b" ] labels;
          "b")
    in
    List.iter
      (fun (m : Play.move) ->
        if m.player = P1 && m.action <> "b" then Alcotest.fail "seat overridden")
      r.moves;
    if not (List.mem (Float.abs r.payoff) [ 1.; 2. ]) then
      Alcotest.failf "payoff %g" r.payoff
  done

let replay () =
  let run seed =
    let rng = Rng.make seed in
    List.init 50 (fun _ ->
        (P.hand ~rng ~seat:P0 ~bot:(Kuhn.equilibrium 0.) ~ask:(fun _ _ -> "p"))
          .moves)
  in
  Alcotest.(check bool) "same seed" true (run 7 = run 7);
  Alcotest.(check bool) "other seed" false (run 7 = run 8)

let bad_action () =
  match
    P.hand ~rng:(Rng.make 1) ~seat:P0 ~bot:Strategy.empty ~ask:(fun _ _ -> "x")
  with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "accepted x"

let tests =
  [
    Alcotest.test_case "sampled hands average the game value" `Quick
      equilibrium_value;
    Alcotest.test_case "asks only at the seat's infosets" `Quick
      seat_sees_own_infosets;
    Alcotest.test_case "seeded replay" `Quick replay;
    Alcotest.test_case "rejects unknown actions" `Quick bad_action;
  ]
