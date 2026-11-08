open Cfr_solver
module M = Market
module S = Sequence_form

let cfg ?(hidden = 0) ?(rounds = 1) cards width =
  { M.cards; hidden; width; rounds }

let near ?(eps = 1e-12) msg a b = Alcotest.(check (float eps)) msg a b
let rat = Alcotest.testable Rat.pp Rat.equal

let structure () =
  let f c terminals infosets =
    let (module G) = M.game c in
    let module Check = Game_check.Make (G) in
    let r = Check.check () in
    Alcotest.(check int) (M.name c ^ " terminals") terminals r.terminals;
    Alcotest.(check int) (M.name c ^ " infosets") infosets r.infosets
  in
  f (cfg 3 0) (6 * 3 * 3) (3 + (3 * 3));
  f (cfg ~hidden:1 5 2) (60 * 7 * 3) (5 + (5 * 7));
  f (cfg ~rounds:2 4 1)
    (12 * 5 * 3 * 5 * 3)
    (4 + (4 * 5) + (4 * 5 * 3) + (4 * 5 * 3 * 5))

let payoffs () =
  let module G = M.Make (struct
    let config = cfg 3 0
  end) in
  let module Check = Game_check.Make (G) in
  near "maker sells 3 at 4" 1. (Check.payoff [ "0"; "4"; "buy" ]);
  near "pass" 0. (Check.payoff [ "0"; "5"; "pass" ]);
  Alcotest.(check string)
    "taker sees card and quote" "2:4"
    (Check.infoset [ "0"; "4" ]);
  let module G2 = M.Make (struct
    let config = cfg ~rounds:2 3 1
  end) in
  let module Check2 = Game_check.Make (G2) in
  near "two rounds" (-3.) (Check2.payoff [ "0"; "4"; "sell"; "5"; "buy" ]);
  Alcotest.(check string)
    "maker sees round one" "1:4s,5"
    (Check2.infoset [ "0"; "4"; "sell"; "5" ])

let ranges () =
  Alcotest.(check (pair int int)) "two of ten" (3, 19) (M.mids (cfg 10 0));
  Alcotest.(check (pair int int))
    "three of ten" (6, 27)
    (M.mids (cfg ~hidden:1 10 0));
  near "fair, own 1 of 3" 3.5 (M.fair (cfg 3 0) 1);
  near "fair with a hidden card"
    (10. +. (2. *. 45. /. 9.))
    (M.fair (cfg ~hidden:1 10 0) 10);
  near "buy pays the ask" (-1.5) (M.pnl (cfg 3 3) Buy 4 4);
  near "sell gets the bid" 0.5 (M.pnl (cfg 3 1) Sell 4 3)

let revealing_by_hand () =
  let c = cfg 3 0 in
  let (module G) = M.game c in
  let module B = Best_response.Make (G) in
  near "taker wins 2/3" (2. /. 3.) (B.value P1 (M.revealing c))

let wide_market_is_free () =
  List.iter
    (fun c ->
      let sol = S.solve (S.build (M.game c)) in
      Alcotest.check rat (M.name c) Rat.zero sol.value)
    [ cfg 4 10; cfg ~hidden:1 5 12 ]

let lp_against_cfr () =
  List.iter
    (fun c ->
      let g = M.game c in
      let (module G) = g in
      let module C = Cfr.Make (G) in
      let module B = Best_response.Make (G) in
      let sol = S.solve (S.build g) in
      let t = C.create ~algorithm:Cfr.dcfr () in
      C.run t 2000;
      let avg = C.average_strategy t in
      let nc = B.nash_conv avg in
      if nc > 1e-3 then Alcotest.failf "%s: nash conv %g" (M.name c) nc;
      let v = Rat.to_float sol.value in
      if -.B.value P1 avg > v +. 1e-9 || B.value P0 avg < v -. 1e-9 then
        Alcotest.failf "%s: lp value %g outside the cfr bracket" (M.name c) v;
      let t = S.build g and x, y = sol.plans in
      Alcotest.check rat "lp is exact" Rat.zero
        (Rat.add (S.best_response t P0 y) (S.best_response t P1 x)))
    [ cfg 3 0; cfg 4 1; cfg ~hidden:1 5 2; cfg ~rounds:2 3 1 ]

let leakage () =
  let c = cfg 6 3 in
  let g = M.game c in
  let (module G) = g in
  let module B = Best_response.Make (G) in
  let eq = Rat.to_float (S.solve (S.build g)).value in
  let reveal = -.B.value P1 (M.revealing c)
  and blind = -.B.value P1 (M.uninformed c) in
  if not (eq > reveal && eq > blind) then
    Alcotest.failf "equilibrium %g, revealing %g, uninformed %g" eq reveal blind

let rejects () =
  Alcotest.check_raises "deck"
    (Invalid_argument "Market: need at least 2 + hidden cards") (fun () ->
      ignore (M.game (cfg ~hidden:1 2 0)));
  Alcotest.check_raises "rounds"
    (Invalid_argument "Market: rounds must be 1 or 2") (fun () ->
      ignore (M.game (cfg ~rounds:3 4 0)))

let tests =
  [
    Alcotest.test_case "tree shape" `Quick structure;
    Alcotest.test_case "payoffs and infosets" `Quick payoffs;
    Alcotest.test_case "ranges and pnl" `Quick ranges;
    Alcotest.test_case "revealing quote by hand" `Quick revealing_by_hand;
    Alcotest.test_case "wide market is worth 0" `Quick wide_market_is_free;
    Alcotest.test_case "lp inside the cfr bracket" `Quick lp_against_cfr;
    Alcotest.test_case "mixing beats revealing" `Quick leakage;
    Alcotest.test_case "bad configs" `Quick rejects;
  ]
