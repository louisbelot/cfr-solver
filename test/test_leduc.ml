open Cfr_solver
module Check = Game_check.Make (Leduc)
module B = Best_response.Make (Leduc)
module E = Eval.Make (Leduc)

let structure () =
  let c = Check.check () in
  Alcotest.(check int)
    "histories"
    (1 + 3 + (9 * 15) + (24 * 5 * 15))
    c.histories;
  Alcotest.(check int) "terminals" ((9 * 4) + (24 * 5 * 9)) c.terminals;
  Alcotest.(check int) "infosets" 288 c.infosets;
  Alcotest.(check int) "per player" 144 (List.length (E.infosets P0))

let kq = [ "2"; "1" ]

let payoffs () =
  let f name expected path =
    Alcotest.(check (float 0.)) name expected (Check.payoff path)
  in
  f "raise, fold" 1. (kq @ [ "r"; "f" ]);
  f "check, raise, fold" (-1.) (kq @ [ "c"; "r"; "f" ]);
  f "re-raise, fold" (-3.) (kq @ [ "r"; "r"; "f" ]);
  f "high card" 5. (kq @ [ "r"; "r"; "c"; "0"; "c"; "c" ]);
  f "pair beats a king" (-5.) (kq @ [ "r"; "r"; "c"; "1"; "c"; "c" ]);
  f "biggest pot" 13. (kq @ [ "r"; "r"; "c"; "0"; "r"; "r"; "c" ]);
  f "round 2 fold" 1. (kq @ [ "c"; "c"; "0"; "r"; "f" ]);
  f "round 2 re-raise, fold" (-5.) (kq @ [ "c"; "c"; "0"; "r"; "r"; "f" ]);
  f "split pot" 0. [ "1"; "1"; "c"; "c"; "0"; "c"; "c" ];
  f "jack pairs the board" 1. [ "0"; "2"; "c"; "c"; "0"; "c"; "c" ]

let keys () =
  let s = Alcotest.(check string) in
  s "player 0 opens" "K:" (Check.infoset kq);
  s "player 1 sees its own card" "Q:c" (Check.infoset (kq @ [ "c" ]));
  s "round 2" "KJ:rrc/" (Check.infoset (kq @ [ "r"; "r"; "c"; "0" ]));
  s "round 2, player 1" "QJ:rrc/c"
    (Check.infoset (kq @ [ "r"; "r"; "c"; "0"; "c" ]))

let legal () =
  let labels path =
    match Leduc.node (Check.follow Leduc.root path) with
    | Game.Decision { actions; _ } -> List.map fst actions
    | _ -> Alcotest.fail "not a decision"
  in
  let l = Alcotest.(check (list string)) in
  l "no fold when nothing to call" [ "c"; "r" ] (labels kq);
  l "facing a raise" [ "f"; "c"; "r" ] (labels (kq @ [ "r" ]));
  l "raise cap" [ "f"; "c" ] (labels (kq @ [ "c"; "r"; "r" ]))

let openspiel_nash_conv () =
  Alcotest.(check (float 1e-12))
    "uniform" 4.747222222222222
    (B.nash_conv Strategy.empty);
  let first =
    List.fold_left
      (fun s (key, actions) ->
        let probs = Array.mapi (fun a _ -> if a = 0 then 1. else 0.) actions in
        Strategy.add key ~actions ~probs s)
      Strategy.empty
      (E.infosets P0 @ E.infosets P1)
  in
  Alcotest.(check (float 1e-12)) "first action" 2. (B.nash_conv first)

module C = Cfr.Make (Leduc)

let cfr_plus_solves_leduc () =
  let t = C.create ~algorithm:Plus () in
  C.run t 500;
  let s = C.average_strategy t in
  Alcotest.(check (float 1e-4)) "value" (-0.085606) (E.expected_value s);
  let e = B.exploitability s in
  if e > 2e-3 then Alcotest.failf "exploitability %g after 500" e

let big () =
  let n = 13 in
  let module G = (val Leduc.game { ranks = n; suits = 2 }) in
  let module C = Game_check.Make (G) in
  let c = C.check () in
  Alcotest.(check string) "name" "leduc13" G.name;
  Alcotest.(check int) "infosets" ((6 * n) + (30 * n * n)) c.infosets;
  Alcotest.(check int)
    "histories"
    (1 + n + (n * n * 15) + (((n * n * n) - n) * 75))
    c.histories

let three_suits () =
  let module G = (val Leduc.game { ranks = 4; suits = 3 }) in
  let module C = Game_check.Make (G) in
  let c = C.check () in
  Alcotest.(check int) "histories" (1 + 4 + (16 * 15) + (64 * 75)) c.histories;
  Alcotest.(check (float 0.))
    "both pair the board" 0.
    (C.payoff [ "0"; "0"; "r"; "c"; "0"; "r"; "c" ]);
  Alcotest.(check (float 0.))
    "ten pairs, beats a king" 7.
    (C.payoff [ "0"; "3"; "r"; "c"; "0"; "r"; "c" ]);
  Alcotest.(check string) "key" "KT:rc/" (C.infoset [ "3"; "0"; "r"; "c"; "0" ])

let names () =
  let name d r = Leduc.rank_name d r in
  let s = Alcotest.(check string) in
  s "standard" "J" (name Leduc.standard 0);
  s "standard king" "K" (name Leduc.standard 2);
  s "deuce" "2" (name { ranks = 13; suits = 2 } 0);
  s "ace" "A" (name { ranks = 13; suits = 2 } 12);
  Alcotest.check_raises "too many ranks"
    (Invalid_argument "Leduc.game: 1 to 13 ranks") (fun () ->
      ignore (Leduc.game { ranks = 14; suits = 2 }));
  Alcotest.check_raises "too few cards"
    (Invalid_argument "Leduc.game: too few cards") (fun () ->
      ignore (Leduc.game { ranks = 2; suits = 1 }))

let tests =
  [
    Alcotest.test_case "13 ranks" `Quick big;
    Alcotest.test_case "three suits" `Quick three_suits;
    Alcotest.test_case "rank names and limits" `Quick names;
    Alcotest.test_case "tree shape and contract" `Quick structure;
    Alcotest.test_case "payoffs" `Quick payoffs;
    Alcotest.test_case "infoset keys" `Quick keys;
    Alcotest.test_case "legal actions" `Quick legal;
    Alcotest.test_case "OpenSpiel nash conv" `Quick openspiel_nash_conv;
    Alcotest.test_case "CFR+ solves Leduc" `Quick cfr_plus_solves_leduc;
  ]
