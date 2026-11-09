open Cfr_solver

let config ?(order = Goofspiel.Descending) ?(payoff = Goofspiel.Win_loss)
    ?(reveal = false) cards =
  { Goofspiel.cards; order; payoff; reveal }

let infosets c = Array.length (Tree.build (Goofspiel.game c)).infosets

let counts c =
  let (module G : Game.S) = Goofspiel.game c in
  let module C = Game_check.Make (G) in
  C.check ()

let rec perm n t = if t = 0 then 1 else n * perm (n - 1) (t - 1)

let openspiel_sizes () =
  Alcotest.(check int) "goofspiel 5, descending" 2124 (infosets (config 5));
  Alcotest.(check int)
    "goofspiel 4, random" 3608
    (infosets (config ~order:Random 4))

let structure () =
  List.iter
    (fun (c, terminals) ->
      let k = counts c in
      Alcotest.(check int)
        (Goofspiel.name c ^ " terminals")
        terminals k.terminals)
    [
      (config 4, 576);
      (config ~order:Ascending 4, 576);
      (config ~order:Random 4, 13824);
      (config ~reveal:true ~order:Random 3, 216);
      (config 1, 1);
      (config ~order:Random 2, 8);
    ];
  List.iter
    (fun n ->
      let open_ =
        2
        * List.fold_left ( + ) 0
            (List.init (n - 1) (fun t -> perm n t * perm n t))
      in
      Alcotest.(check int)
        (Printf.sprintf "open %d" n)
        open_
        (infosets (config ~reveal:true n)))
    [ 2; 3; 4; 5 ]

let payoffs () =
  let c = config ~payoff:Points 3 in
  let (module G : Game.S) = Goofspiel.game c in
  let module C = Game_check.Make (G) in
  let near = Alcotest.(check (float 0.)) in
  near "3>1, 2<3, 1<2" 0. (C.payoff [ "3"; "1"; "2"; "3" ]);
  near "3>2, 1<3, 2>1" 2. (C.payoff [ "3"; "2"; "1"; "3" ]);
  near "tie on 3 discards it" (-1.) (C.payoff [ "3"; "3"; "1"; "2" ]);
  near "ties throughout" 0. (C.payoff [ "1"; "1"; "2"; "2" ]);
  let (module W : Game.S) = Goofspiel.game { c with payoff = Win_loss } in
  let module D = Game_check.Make (W) in
  near "win" 1. (D.payoff [ "3"; "2"; "1"; "3" ]);
  near "loss" (-1.) (D.payoff [ "3"; "3"; "1"; "2" ]);
  near "draw" 0. (D.payoff [ "3"; "1"; "2"; "3" ]);
  let (module R : Game.S) = Goofspiel.game { c with order = Random } in
  let module E = Game_check.Make (R) in
  near "upcards 1 then 3" 2. (E.payoff [ "0"; "1"; "3"; "1"; "3"; "1" ])

let keys () =
  let (module G : Game.S) = Goofspiel.game (config 3) in
  let module C = Game_check.Make (G) in
  let s = Alcotest.(check string) in
  s "root" "0|3||" (C.infoset []);
  s "p1 does not see the pending bid" "1|3||" (C.infoset [ "2" ]);
  s "outcome only" "0|3,2|2|W" (C.infoset [ "2"; "1" ]);
  s "losing bid hidden" (C.infoset [ "3"; "1" ]) (C.infoset [ "3"; "2" ]);
  s "opponent side" "1|3,2|1|L" (C.infoset [ "2"; "1"; "3" ]);
  let (module O : Game.S) = Goofspiel.game (config ~reveal:true 3) in
  let module D = Game_check.Make (O) in
  s "revealed bids" "1|3,2|1|2" (D.infoset [ "2"; "1"; "3" ])

let rat = Alcotest.testable Rat.pp Rat.equal

let lp_value () =
  List.iter
    (fun c ->
      let t = Sequence_form.build (Goofspiel.game c) in
      let sol = Sequence_form.solve t in
      Alcotest.check rat (Goofspiel.name c) Rat.zero sol.value;
      Alcotest.check rat "nash conv" Rat.zero
        (Sequence_form.nash_conv t sol.strategy))
    [ config 3; config ~payoff:Points 3; config ~order:Random ~payoff:Points 3 ]

let cfr_converges () =
  let (module G : Game.S) = Goofspiel.game (config 4) in
  let module C = Cfr.Make (G) in
  let module B = Best_response.Make (G) in
  let c = C.create ~algorithm:Cfr.pcfr_plus () in
  C.run c 10;
  let early = B.nash_conv (C.average_strategy c) in
  C.run c 490;
  let late = B.nash_conv (C.average_strategy c) in
  Alcotest.(check bool) "improves" true (late < early /. 10.);
  Alcotest.(check bool) "close" true (late < 1e-3)

let tests =
  [
    Alcotest.test_case "openspiel infoset counts" `Quick openspiel_sizes;
    Alcotest.test_case "tree shape and contract" `Quick structure;
    Alcotest.test_case "payoffs" `Quick payoffs;
    Alcotest.test_case "infoset keys" `Quick keys;
    Alcotest.test_case "lp value is zero" `Quick lp_value;
    Alcotest.test_case "pcfr+ converges" `Quick cfr_converges;
  ]
