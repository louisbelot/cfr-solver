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

let bit k = 1 lsl (k - 1)
let set = List.fold_left (fun m k -> m lor bit k) 0
let rat = Alcotest.testable Rat.pp Rat.equal

let rhoads_example () =
  let t = Gops.Exact.create Random in
  let x, y, v =
    Gops.Exact.turn t (set [ 2; 4 ]) (set [ 1; 3 ]) (set [ 12; 13 ]) 13
  in
  Alcotest.check rat "12.52" (Rat.of_ints 313 25) v;
  Alcotest.check rat "high with 52%" (Rat.of_ints 13 25) x.(1);
  Alcotest.check rat "her 3 with 48%" (Rat.of_ints 12 25) y.(1)

let table1 =
  [|
    [| 0.0470; 0.1855; 0.1182; 0.1226; 0.1123 |];
    [| 0.8327; 0.; 0.1188; 0.07347; 0.0241 |];
    [| 0.1203; 0.7375; 0.; 0.1915; 0. |];
    [| 0.; 0.0770; 0.7630; 0.2043; 0. |];
    [| 0.; 0.; 0.; 0.4081; 0.8636 |];
  |]

let first_move () =
  let t = Gops.Float.create Random and all = set [ 1; 2; 3; 4; 5 ] in
  for k = 1 to 5 do
    let x, y, _ = Gops.Float.turn t all all all k in
    for b = 1 to 5 do
      Alcotest.(check (float 6e-5))
        (Printf.sprintf "upcard %d, bid %d" k b)
        table1.(b - 1).(k - 1)
        x.(b - 1);
      Alcotest.(check (float 1e-9)) "symmetric" x.(b - 1) y.(b - 1)
    done
  done;
  Alcotest.(check (float 1e-12)) "value" 0. (Gops.Float.value t all all all)

let paper_num =
  "5306287082133275981487303358632452609704399851215456295628045583"
  ^ "1875882988814425504038999635791836938252991208618766205613347715"
  ^ "8328568084730206912059640079498714570709437787806964249018639429"
  ^ "44810525052774918079399954562997646185715598682274851480804900"

let paper_den =
  "1129268720669360758902051860285520214948806792442363910626460283"
  ^ "5249749724573405290487422550704989332508431906657764350941879381"
  ^ "7660750019182194563647083966290489533469158285998067618020193436"
  ^ "7037256681898791738381922918288920527729219085504200644992335857"

let exact_entry () =
  let t = Gops.Exact.create Random and all = set [ 1; 2; 3; 4; 5 ] in
  let x, _, _ = Gops.Exact.turn t all all all 1 in
  Alcotest.(check string)
    "upcard 1, bid 1"
    (paper_num ^ "/" ^ paper_den)
    (Rat.to_string x.(0))

let exact_float () =
  List.iter
    (fun order ->
      let e = Gops.Exact.create order and f = Gops.Float.create order in
      let all = set [ 1; 2; 3; 4 ] in
      for v = 1 to 15 do
        for y = 1 to 15 do
          for p = 1 to 15 do
            let size m = List.length (Goofspiel.members m) in
            if size v = size y && size y = size p then
              Alcotest.(check (float 1e-9))
                "subgame value"
                (Rat.to_float (Gops.Exact.value e v y p))
                (Gops.Float.value f v y p)
          done
        done
      done;
      Alcotest.check rat "root" Rat.zero (Gops.Exact.value e all all all))
    [ Goofspiel.Descending; Ascending; Random ]

let antisymmetry () =
  let t = Gops.Exact.create Random in
  let a =
    Gops.Exact.value t (set [ 1; 3; 5 ]) (set [ 2; 3; 4 ]) (set [ 1; 2; 5 ])
  in
  let u = Gops.Exact.create Random in
  let b =
    Gops.Exact.value u (set [ 2; 3; 4 ]) (set [ 1; 3; 5 ]) (set [ 1; 2; 5 ])
  in
  Alcotest.check rat "f(V,Y,P) = -f(Y,V,P)" a (Rat.neg b);
  Alcotest.(check bool) "nonzero" false (Rat.is_zero a)

let dp_equilibrium () =
  List.iter
    (fun c ->
      let (module G : Game.S) = Goofspiel.game c in
      let module B = Best_response.Make (G) in
      let s = Gops.strategy c in
      Alcotest.(check (float 1e-9)) (Goofspiel.name c) 0. (B.nash_conv s))
    [
      config ~reveal:true ~payoff:Points ~order:Random 4;
      config ~reveal:true ~payoff:Points 5;
      config ~reveal:true ~payoff:Points ~order:Ascending 4;
    ]

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
    Alcotest.test_case "rhoads-bartholdi 12.52" `Quick rhoads_example;
    Alcotest.test_case "rhoads-bartholdi table 1" `Quick first_move;
    Alcotest.test_case "rhoads-bartholdi exact entry" `Quick exact_entry;
    Alcotest.test_case "exact and float dp agree" `Quick exact_float;
    Alcotest.test_case "antisymmetry" `Quick antisymmetry;
    Alcotest.test_case "dp profile is an equilibrium" `Quick dp_equilibrium;
    Alcotest.test_case "lp value is zero" `Quick lp_value;
    Alcotest.test_case "pcfr+ converges" `Quick cfr_converges;
  ]
