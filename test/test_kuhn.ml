open Cfr_solver
module Check = Game_check.Make (Kuhn)

let payoff = Check.payoff
let infoset = Check.infoset

let structure () =
  let c = Check.check () in
  Alcotest.(check int) "histories" 55 c.histories;
  Alcotest.(check int) "terminals" 30 c.terminals;
  Alcotest.(check int) "infosets" 12 c.infosets

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
