open Cfr_solver
module A = Auction

let cfg ?(ties = A.Split) ?(reserve = 0) rule bidders top =
  { A.bidders; top; rule; ties; reserve }

let rules = [ A.First_price; Second_price; All_pay ]

let small =
  List.concat_map
    (fun rule ->
      List.concat_map
        (fun ties ->
          [
            cfg ~ties rule 2 3;
            cfg ~ties ~reserve:2 rule 2 3;
            cfg ~ties rule 3 2;
            cfg ~ties ~reserve:1 rule 3 2;
          ])
        [ A.Split; Void ])
    rules

let random_profile st (c : A.config) =
  Array.init c.bidders (fun _ ->
      Array.init (c.top + 1) (fun _ ->
          let w = Array.init (c.top + 1) (fun _ -> Random.State.float st 1.) in
          if Random.State.bool st then w.(Random.State.int st (c.top + 1)) <- 5.;
          Regret_matching.normalize w))

let near ?(eps = 1e-12) msg a b = Alcotest.(check (float eps)) msg a b
let rat = Alcotest.testable Rat.pp Rat.equal
let q = Rat.of_ints
let floor_half x = Array.init (x + 1) (fun v -> v / 2)
let ceil_half x = Array.init (x + 1) (fun v -> (v + 1) / 2)
let truthful x = Array.init (x + 1) Fun.id

let tree_values () =
  let st = Random.State.make [| 7 |] in
  List.iter
    (fun c ->
      let (module G) = A.game c in
      let module E = Multi.Eval (G) in
      let module B = Multi_br.Make (G) in
      for _ = 1 to 5 do
        let s = random_profile st c in
        let strat = A.strategy c s in
        let tree = E.values strat and gains = B.gains strat in
        Array.iteri
          (fun i v ->
            near (A.name c ^ " value") v (A.Approx.value c s i);
            near (A.name c ^ " gain") gains.(i) (A.Approx.gains c s).(i))
          tree
      done)
    small

let truthful_dominant () =
  List.iter
    (fun c ->
      let s = A.Exact.symmetric c (truthful c.A.top) in
      Alcotest.(check bool) (A.name c) true (A.Exact.is_equilibrium c s))
    [
      cfg Second_price 2 6;
      cfg ~ties:Void Second_price 2 6;
      cfg ~reserve:3 Second_price 2 6;
      cfg Second_price 3 4;
      cfg ~ties:Void ~reserve:2 Second_price 4 3;
    ];
  let st = Random.State.make [| 11 |] in
  for _ = 1 to 20 do
    let c = cfg ~reserve:(Random.State.int st 3) Second_price 3 4 in
    let s = random_profile st c in
    let _, bids = A.Approx.best c s 0 in
    let u = A.Approx.utilities c s 0 in
    Array.iteri
      (fun v b ->
        near "bidding the value is a best response" u.(v).(b) u.(v).(v))
      bids
  done

let second_price_revenue () =
  List.iter
    (fun x ->
      let c = cfg Second_price 2 x in
      let expected =
        List.init x (fun k ->
            let p = q (x - k) (x + 1) in
            Rat.mul p p)
        |> List.fold_left Rat.add Rat.zero
      in
      Alcotest.check rat "E[min of two]" expected
        (A.Exact.revenue c (A.Exact.symmetric c (truthful x))))
    [ 1; 4; 9 ]

let two_bidders_without_ties () =
  for x = 2 to 7 do
    let found =
      A.Exact.symmetric_pure_equilibria (cfg ~ties:Void First_price 2 x)
    in
    let expected = List.sort_uniq compare [ floor_half x; ceil_half x ] in
    Alcotest.(check (list (array int)))
      (Printf.sprintf "x = %d" x)
      expected (List.sort compare found)
  done

let three_bidders_without_ties () =
  for x = 4 to 6 do
    Alcotest.(check int)
      (Printf.sprintf "x = %d" x)
      0
      (List.length
         (A.Exact.symmetric_pure_equilibria (cfg ~ties:Void First_price 3 x)))
  done

let floor_revenue () =
  let c = cfg ~ties:Void First_price 2 40 in
  let s = A.Approx.symmetric c (floor_half 40) in
  near ~eps:0.02 "close to 1/3" (1. /. 3.) (A.Approx.revenue c s /. 40.);
  near ~eps:1e-12 "exact equilibrium" 0. (A.Approx.nash_conv c s)

let theory () =
  near "n = 2" (1. /. 3.) (A.Theory.revenue ~bidders:2 ());
  near "myerson" (5. /. 12.) (A.Theory.revenue ~bidders:2 ~reserve:0.5 ());
  near "n = 5" (4. /. 6.) (A.Theory.revenue ~bidders:5 ());
  near "first price" 0.5 (A.Theory.bid First_price ~bidders:2 1.);
  near "all pay" (2. /. 3. *. 0.125) (A.Theory.bid All_pay ~bidders:3 0.5);
  near "lowest entrant bids the reserve" 0.3
    (A.Theory.bid First_price ~bidders:4 ~reserve:0.3 0.3);
  let simpson f a b =
    let k = 2000 in
    let h = (b -. a) /. float_of_int k in
    let acc = ref (f a +. f b) in
    for i = 1 to k - 1 do
      let w = if i mod 2 = 1 then 4. else 2. in
      acc := !acc +. (w *. f (a +. (h *. float_of_int i)))
    done;
    !acc *. h /. 3.
  in
  List.iter
    (fun (n, r) ->
      let nf = float_of_int n in
      let pay v =
        nf
        *. A.Theory.bid First_price ~bidders:n ~reserve:r v
        *. Float.pow v (nf -. 1.)
      in
      near ~eps:1e-9 "revenue equivalence" (simpson pay r 1.)
        (A.Theory.revenue ~bidders:n ~reserve:r ()))
    [ (2, 0.); (2, 0.5); (3, 0.2); (5, 0.7) ]

let matches_tree_cfr () =
  List.iter
    (fun (c, algorithm) ->
      let (module G) = A.game c in
      let module C = Multi_cfr.Make (G) in
      let tree = C.create ~algorithm () in
      let fast = A.Solver.create ~algorithm c in
      C.run tree 30;
      A.Solver.run fast 30;
      let a = A.profile c (C.average_strategy tree)
      and b = A.Solver.average fast in
      Array.iteri
        (fun i rows ->
          Array.iteri
            (fun v row ->
              Array.iteri
                (fun k x -> near ~eps:1e-10 (A.name c) x b.(i).(v).(k))
                row)
            rows)
        a)
    [
      (cfg First_price 2 3, Cfr.Vanilla);
      (cfg ~ties:Void ~reserve:1 Second_price 2 3, Cfr.Plus);
      (cfg All_pay 3 2, Cfr.dcfr);
      (cfg ~ties:Void First_price 3 2, Cfr.Linear);
    ]

let converges () =
  List.iter
    (fun (c, bound) ->
      let t = A.Solver.create ~algorithm:Cfr.Plus c in
      A.Solver.run t 3000;
      let nc = A.Approx.nash_conv c (A.Solver.average t) in
      if nc > bound then Alcotest.failf "%s: nash conv %g" (A.name c) nc)
    [
      (cfg ~ties:Void First_price 2 10, 1e-3);
      (cfg First_price 2 10, 5e-3);
      (cfg Second_price 3 8, 1e-4);
      (cfg All_pay 2 10, 1e-3);
    ]

let exact_last_iterate () =
  List.iter
    (fun (c, n) ->
      let t = A.Solver.create ~algorithm:Cfr.Plus c in
      A.Solver.run t n;
      let q = A.rationalize (A.Solver.current t) in
      Alcotest.check rat (A.name c) Rat.zero (A.Exact.nash_conv c q))
    [
      (cfg ~ties:Void First_price 2 10, 2000);
      (cfg ~ties:Void First_price 3 6, 20000);
    ]

let irrational_mix () =
  let c = cfg ~ties:Void First_price 3 12 in
  let t = A.Solver.create ~symmetric:true c in
  A.Solver.run t 20000;
  let s = A.Solver.current t in
  near ~eps:1e-9 "bid 2 at v = 4" ((3. *. Float.sqrt 2.) -. 4.) s.(0).(4).(2);
  List.iter
    (fun v -> near (Printf.sprintf "2v/3 at %d" v) 1. s.(0).(v).(2 * v / 3))
    [ 3; 6; 9; 12 ];
  let nc = Rat.to_float (A.Exact.nash_conv c (A.rationalize s)) in
  if nc > 1e-12 then Alcotest.failf "exact nash conv %g" nc

let rejects () =
  Alcotest.check_raises "one bidder"
    (Invalid_argument "Auction: need at least two bidders") (fun () ->
      ignore (A.game (cfg First_price 1 3)));
  Alcotest.check_raises "reserve"
    (Invalid_argument "Auction: reserve must be in [0, top]") (fun () ->
      ignore (A.Solver.create (cfg ~reserve:4 First_price 2 3)))

let tests =
  [
    Alcotest.test_case "tree values and gains" `Quick tree_values;
    Alcotest.test_case "truthful second price" `Quick truthful_dominant;
    Alcotest.test_case "second price revenue" `Quick second_price_revenue;
    Alcotest.test_case "two bidders, floor and ceil" `Quick
      two_bidders_without_ties;
    Alcotest.test_case "three bidders, none symmetric" `Quick
      three_bidders_without_ties;
    Alcotest.test_case "floor revenue near 1/3" `Quick floor_revenue;
    Alcotest.test_case "continuous theory" `Quick theory;
    Alcotest.test_case "same iterates as tree cfr" `Quick matches_tree_cfr;
    Alcotest.test_case "cfr+ converges" `Quick converges;
    Alcotest.test_case "exact last iterates" `Quick exact_last_iterate;
    Alcotest.test_case "mixing at 3 sqrt 2 - 4" `Quick irrational_mix;
    Alcotest.test_case "bad configs" `Quick rejects;
  ]
