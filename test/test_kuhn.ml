open Cfr_solver

(* Follow a path from the root: chance outcomes by index, actions by label. *)
let rec follow h = function
  | [] -> h
  | step :: rest -> (
      match Kuhn.node h with
      | Game.Chance outcomes ->
          follow (snd (List.nth outcomes (int_of_string step))) rest
      | Decision { actions; _ } -> follow (List.assoc step actions) rest
      | Terminal _ -> Alcotest.fail "path runs past a terminal")

let payoff path =
  match Kuhn.node (follow Kuhn.root path) with
  | Game.Terminal u -> u
  | _ -> Alcotest.failf "%s is not terminal" (String.concat " " path)

let infoset path =
  match Kuhn.node (follow Kuhn.root path) with
  | Game.Decision { infoset; _ } -> infoset
  | _ -> Alcotest.fail "not a decision"

module Check = Game_check.Make (Kuhn)

let structure () =
  let c = Check.check () in
  Alcotest.(check int) "histories" 55 c.histories;
  Alcotest.(check int) "terminals" 30 c.terminals;
  Alcotest.(check int) "infosets" 12 c.infosets

(* Deal 0 is (J, Q) and deal 5 is (K, Q). *)
let payoffs () =
  let f = Alcotest.(check (float 0.)) in
  f "J vs Q, check-check" (-1.) (payoff [ "0"; "p"; "p" ]);
  f "K vs Q, check-check" 1. (payoff [ "5"; "p"; "p" ]);
  f "bet, fold" 1. (payoff [ "0"; "b"; "p" ]);
  f "check, bet, fold" (-1.) (payoff [ "5"; "p"; "b"; "p" ]);
  f "J vs Q, bet-call" (-2.) (payoff [ "0"; "b"; "b" ]);
  f "K vs Q, check-bet-call" 2. (payoff [ "5"; "p"; "b"; "b" ])

let keys () =
  let s = Alcotest.(check string) in
  s "player 0 opens" "J" (infoset [ "0" ]);
  s "player 1 sees only its card" "Qp" (infoset [ "0"; "p" ]);
  s "player 0 facing a bet" "Kpb" (infoset [ "5"; "p"; "b" ])

let tests =
  [
    Alcotest.test_case "tree shape and contract" `Quick structure;
    Alcotest.test_case "payoffs" `Quick payoffs;
    Alcotest.test_case "infoset keys" `Quick keys;
  ]
