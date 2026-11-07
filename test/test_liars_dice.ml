open Cfr_solver

let standard = Liars_dice.standard
let nowild = { standard with wild = false }

module D6 = Liars_dice.Make (struct
  let config = standard
end)

module Check = Game_check.Make (D6)

let deal r0 r1 = string_of_int (((r0 - 1) * 6) + (r1 - 1))

let structure () =
  let f config m =
    let (module G : Game.S) = Liars_dice.game config in
    let module C = Game_check.Make (G) in
    let c = C.check () in
    let b = 1 lsl Liars_dice.bids config in
    let name = G.name in
    Alcotest.(check int)
      (name ^ " histories")
      (1 + (m * m * ((2 * b) - 1)))
      c.histories;
    Alcotest.(check int) (name ^ " terminals") (m * m * (b - 1)) c.terminals;
    Alcotest.(check int) (name ^ " infosets") (m * b) c.infosets
  in
  f { dice = 1; faces = 3; wild = true } 3;
  f { dice = 1; faces = 4; wild = false } 4;
  f { dice = 2; faces = 2; wild = true } 3;
  f { dice = 3; faces = 2; wild = false } 4

let openspiel_size () =
  Alcotest.(check int)
    "24,576 infosets" 24_576
    (Array.length (Tree.build (module D6)).infosets)

let rolls () =
  let near = Alcotest.(check (float 1e-12)) in
  let config = { standard with dice = 2 } in
  let rs = Liars_dice.rolls config in
  Alcotest.(check int) "21 multisets of two d6" 21 (List.length rs);
  near "sum" 1. (List.fold_left (fun acc (p, _) -> acc +. p) 0. rs);
  near "double" (1. /. 36.)
    (List.assoc [ 1; 1 ] (List.map (fun (p, r) -> (r, p)) rs));
  near "mixed" (2. /. 36.)
    (List.assoc [ 2; 5 ] (List.map (fun (p, r) -> (r, p)) rs));
  let three = Liars_dice.rolls { dice = 3; faces = 4; wild = true } in
  near "three distinct" (6. /. 64.)
    (List.assoc [ 1; 2; 3 ] (List.map (fun (p, r) -> (r, p)) three));
  Alcotest.(check bool)
    "sorted" true
    (List.for_all (fun (_, r) -> List.sort compare r = r) three)

let holds () =
  let b q f = ((q - 1) * 6) + (f - 1) in
  let h config r0 r1 bid = Liars_dice.holds config r0 r1 bid in
  let t = Alcotest.(check bool) in
  t "one three" true (h standard [ 3 ] [ 5 ] (b 1 3));
  t "two threes" false (h standard [ 3 ] [ 5 ] (b 2 3));
  t "six is wild" true (h standard [ 3 ] [ 6 ] (b 2 3));
  t "not without wilds" false (h nowild [ 3 ] [ 6 ] (b 2 3));
  t "sixes count only sixes" false (h standard [ 3 ] [ 6 ] (b 2 6));
  t "two sixes" true (h standard [ 6 ] [ 6 ] (b 2 6));
  t "labels" true
    (Liars_dice.bid_to_string standard (b 2 3) = "2x3"
    && Liars_dice.bid_to_string standard 0 = "1x1")

let payoffs () =
  let f = Alcotest.(check (float 0.)) in
  f "false bid called" (-1.) (Check.payoff [ deal 3 5; "2x3"; "liar" ]);
  f "wild makes it true" 1. (Check.payoff [ deal 3 6; "2x3"; "liar" ]);
  f "player 1 bluffs" 1. (Check.payoff [ deal 3 5; "1x2"; "1x4"; "liar" ]);
  f "player 1 is right" (-1.) (Check.payoff [ deal 4 5; "1x2"; "1x4"; "liar" ]);
  f "top bid holds" (-1.) (Check.payoff [ deal 6 6; "1x1"; "2x6"; "liar" ])

let keys () =
  let s = Alcotest.(check string) in
  s "opening" "3:" (Check.infoset [ deal 3 5 ]);
  s "player 1" "5:2x3" (Check.infoset [ deal 3 5; "2x3" ]);
  s "history" "3:1x2,1x4" (Check.infoset [ deal 3 5; "1x2"; "1x4" ])

let legal () =
  let labels path =
    match D6.node (Check.follow D6.root path) with
    | Game.Decision { actions; _ } -> List.map fst actions
    | _ -> Alcotest.fail "not a decision"
  in
  let opening = labels [ deal 1 1 ] in
  Alcotest.(check int) "twelve openings" 12 (List.length opening);
  Alcotest.(check bool) "no call to open" false (List.mem "liar" opening);
  Alcotest.(check (list string))
    "after 2x5" [ "2x6"; "liar" ]
    (labels [ deal 1 1; "2x5" ]);
  Alcotest.(check (list string))
    "after 2x6" [ "liar" ]
    (labels [ deal 1 1; "2x6" ])

let solve config iterations =
  let (module G : Game.S) = Liars_dice.game config in
  let module C = Cfr.Make (G) in
  let module B = Best_response.Make (G) in
  let module E = Eval.Make (G) in
  let t = C.create ~algorithm:Cfr.dcfr () in
  C.run t iterations;
  let sigma = C.average_strategy t in
  (E.expected_value sigma, B.exploitability sigma)

let lp_values () =
  List.iter
    (fun (dice, faces, wild, v, bound) ->
      let config = { Liars_dice.dice; faces; wild } in
      let value, expl = solve config 1000 in
      let name = Liars_dice.name config in
      Alcotest.(check (float 1e-5)) (name ^ " value") v value;
      if expl > bound then Alcotest.failf "%s exploitability %g" name expl)
    [
      (1, 2, false, 0., 1e-3);
      (1, 2, true, 1. /. 2., 1e-6);
      (1, 3, false, -1. /. 9., 1e-6);
      (1, 3, true, 1. /. 9., 1e-6);
      (1, 4, false, -1. /. 8., 1e-6);
      (1, 4, true, 1. /. 16., 1e-4);
      (2, 2, false, 1. /. 8., 1e-6);
      (2, 2, true, 7. /. 8., 1e-6);
    ]

let standard_value () =
  let value, expl = solve standard 1000 in
  Alcotest.(check (float 1e-5)) "-7/258" (-7. /. 258.) value;
  if expl > 1e-4 then Alcotest.failf "exploitability %g" expl

let tests =
  [
    Alcotest.test_case "tree shape and contract" `Quick structure;
    Alcotest.test_case "openspiel infoset count" `Quick openspiel_size;
    Alcotest.test_case "roll probabilities" `Quick rolls;
    Alcotest.test_case "bid truth" `Quick holds;
    Alcotest.test_case "payoffs" `Quick payoffs;
    Alcotest.test_case "infoset keys" `Quick keys;
    Alcotest.test_case "legal bids" `Quick legal;
    Alcotest.test_case "lp game values" `Quick lp_values;
    Alcotest.test_case "1d6 value -7/258" `Slow standard_value;
  ]
