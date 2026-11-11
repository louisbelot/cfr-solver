open Cfr_solver
module K3 = (val Kuhn_n.game 3)
module C3 = Multi_cfr.Make (K3)
module B3 = Multi_br.Make (K3)
module E3 = Multi.Eval (K3)

let algorithms = [ Cfr.Vanilla; Plus; Linear; Cfr.dcfr ]

let same_strategy tol a b =
  List.iter2
    (fun (k, x) (k', y) ->
      Alcotest.(check string) "key" k k';
      Array.iteri
        (fun i p ->
          if Float.abs (p -. y.Strategy.probs.(i)) > tol then
            Alcotest.failf "%s: %.15f vs %.15f" k p y.Strategy.probs.(i))
        x.Strategy.probs)
    (Strategy.bindings a) (Strategy.bindings b)

let matches_two_player (module G : Game.S) (module M : Multi.S) n () =
  let module C = Cfr.Make (G) in
  let module N = Multi_cfr.Make (M) in
  List.iter
    (fun algorithm ->
      let c = C.create ~algorithm () and m = N.create ~algorithm () in
      C.run c n;
      N.run m n;
      same_strategy 1e-11 (C.average_strategy c) (N.average_strategy m);
      same_strategy 1e-9 (C.current_strategy c) (N.current_strategy m))
    algorithms

let rec perms k pool =
  if k = 0 then [ [] ]
  else
    List.concat_map
      (fun c ->
        List.map (List.cons c) (perms (k - 1) (List.filter (( <> ) c) pool)))
      pool

let payoff cards moves =
  let deal =
    let rec find i = function
      | [] -> Alcotest.fail "no such deal"
      | d :: _ when d = cards -> i
      | _ :: rest -> find (i + 1) rest
    in
    find 0 (perms 3 [ 0; 1; 2; 3 ])
  in
  let rec follow h = function
    | [] -> (
        match K3.node h with
        | Multi.Terminal u -> u
        | _ -> Alcotest.fail "not terminal")
    | m :: rest -> (
        match K3.node h with
        | Decision { actions; _ } -> follow (List.assoc m actions) rest
        | _ -> Alcotest.fail "runs past a terminal")
  in
  match K3.node K3.root with
  | Chance outcomes ->
      follow
        (snd (List.nth outcomes deal))
        (List.init (String.length moves) (fun i -> String.make 1 moves.[i]))
  | _ -> Alcotest.fail "root is not chance"

let payoffs () =
  let check name expected cards moves =
    Alcotest.(check (array (float 0.))) name expected (payoff cards moves)
  in
  check "three checks" [| -1.; -1.; 2. |] [ 0; 1; 2 ] "ppp";
  check "bet, two folds" [| 2.; -1.; -1. |] [ 3; 1; 2 ] "bpp";
  check "bet, call, fold" [| -1.; -2.; 3. |] [ 0; 1; 2 ] "pbbp";
  check "everyone calls" [| 4.; -2.; -2. |] [ 3; 1; 2 ] "ppbbb";
  check "fold then call" [| -1.; 3.; -2. |] [ 0; 3; 2 ] "ppbpb"

let shape () =
  let nodes = ref 0 and terminals = ref 0 in
  let rec go h =
    incr nodes;
    match K3.node h with
    | Multi.Terminal u ->
        incr terminals;
        if Float.abs (Array.fold_left ( +. ) 0. u) > 0. then
          Alcotest.fail "not zero-sum"
    | Chance outcomes ->
        let total = List.fold_left (fun acc (p, _) -> acc +. p) 0. outcomes in
        Alcotest.(check (float 1e-12)) "chance sums to one" 1. total;
        List.iter (fun (_, h') -> go h') outcomes
    | Decision { actions; _ } -> List.iter (fun (_, h') -> go h') actions
  in
  go K3.root;
  Alcotest.(check int) "histories after the deal" 600 (!nodes - 1);
  Alcotest.(check int) "terminals" 312 !terminals;
  List.iter
    (fun p ->
      Alcotest.(check int)
        "infosets per player" 16
        (List.length (E3.infosets p)))
    [ 0; 1; 2 ];
  Alcotest.(check int) "solver infosets" 48 (C3.infosets (C3.create ()))

let two_player_keys () =
  let module K2 = (val Kuhn_n.game 2) in
  let module E2 = Multi.Eval (K2) in
  let module E = Eval.Make (Kuhn) in
  List.iteri
    (fun i p ->
      Alcotest.(check (list string))
        "same infosets"
        (List.map fst (E.infosets p))
        (List.map fst (E2.infosets i)))
    [ Game.P0; P1 ]

let random_profile =
  QCheck2.Gen.(
    map
      (fun bets ->
        List.fold_left2
          (fun s key b ->
            Strategy.add key ~actions:[| "p"; "b" |] ~probs:[| 1. -. b; b |] s)
          Strategy.empty
          (List.concat_map (fun p -> List.map fst (E3.infosets p)) [ 0; 1; 2 ])
          bets)
      (list_size (return 48) (float_bound_inclusive 1.)))

let brute_force player sigma =
  let by_card = Hashtbl.create 4 in
  List.iter
    (fun (key, _) -> Hashtbl.add by_card key.[0] key)
    (E3.infosets player);
  List.fold_left
    (fun sigma card ->
      let keys = Hashtbl.find_all by_card card in
      let n = List.length keys in
      let candidates =
        List.init (1 lsl n) (fun bits ->
            List.fold_left
              (fun (s, i) key ->
                let b = if bits land (1 lsl i) <> 0 then 1. else 0. in
                ( Strategy.add key ~actions:[| "p"; "b" |]
                    ~probs:[| 1. -. b; b |]
                    s,
                  i + 1 ))
              (sigma, 0) keys
            |> fst)
      in
      List.fold_left
        (fun best s ->
          if (E3.values s).(player) > (E3.values best).(player) then s else best)
        (List.hd candidates) candidates)
    sigma
    (List.sort_uniq compare (Hashtbl.fold (fun c _ acc -> c :: acc) by_card []))
  |> E3.values
  |> fun u -> u.(player)

let best_response_is_best sigma =
  List.for_all
    (fun p ->
      let v = B3.value p sigma in
      let earned =
        (E3.values (Strategy.union (B3.strategy p sigma) sigma)).(p)
      in
      Float.abs (v -. brute_force p sigma) < 1e-12
      && Float.abs (v -. earned) < 1e-12)
    [ 0; 1; 2 ]

let gains_nonnegative sigma =
  Array.for_all (fun g -> g >= -1e-12) (B3.gains sigma)

let qcheck name prop =
  Alcotest.test_case name `Quick (fun () ->
      QCheck2.Test.check_exn
        (QCheck2.Test.make ~count:40 ~name random_profile prop))

let szafron_family () =
  let t = C3.create ~algorithm:Cfr.dcfr () in
  C3.run t 2_000;
  let s = C3.average_strategy t in
  let u = E3.values s and nc = B3.nash_conv s in
  if nc > 1e-4 then Alcotest.failf "nash conv %g" nc;
  Alcotest.(check (float 1e-4)) "player 1 gets -1/48" (-1. /. 48.) u.(1);
  Alcotest.(check (float 1e-12)) "zero-sum" 0. (Array.fold_left ( +. ) 0. u);
  let beta = ((-48. *. u.(0)) -. 1.) /. 2. in
  if beta < -1e-3 || beta > 1. +. 1e-3 then Alcotest.failf "beta = %g" beta;
  Alcotest.(check (float 2e-3))
    "player 2 gets (1 + beta)/24"
    ((1. +. beta) /. 24.)
    u.(2);
  List.iter
    (fun card ->
      let bet = (Strategy.probs s card ~n:2).(1) in
      if bet > 1e-3 then
        Alcotest.failf "player 0 opens %s with a bet %g" card bet)
    [ "T"; "J"; "Q"; "K" ]

let vanilla_is_slow () =
  let nc algorithm =
    let t = C3.create ~algorithm () in
    C3.run t 1_000;
    B3.nash_conv (C3.average_strategy t)
  in
  let vanilla = nc Cfr.Vanilla and dcfr = nc Cfr.dcfr in
  if not (vanilla > 100. *. dcfr) then
    Alcotest.failf "cfr %g vs dcfr %g" vanilla dcfr

let rejects_sizes () =
  Alcotest.check_raises "one player"
    (Invalid_argument "Kuhn_n.game: players must be in [2, 11]") (fun () ->
      ignore (Kuhn_n.game 1));
  Alcotest.check_raises "too few ranks"
    (Invalid_argument "Kuhn_n.game: ranks must be in [players, 12]") (fun () ->
      ignore (Kuhn_n.game ~ranks:2 3))

let ranks () =
  let deals (module G : Multi.S) =
    match G.node G.root with Multi.Chance l -> List.length l | _ -> 0
  in
  Alcotest.(check int)
    "12 x 11 x 10" 1320
    (deals (module (val Kuhn_n.game ~ranks:12 3)));
  Alcotest.(check int)
    "default" 24
    (deals (module (val Kuhn_n.game ~ranks:4 3)));
  let tree = Multi_tree.build (module (val Kuhn_n.game ~ranks:12 3)) in
  Alcotest.(check int) "infosets" 144 (Array.length tree.keys);
  Alcotest.(check bool)
    "lowest card" true
    (Array.exists (String.equal "2") tree.keys)

let tests =
  [
    Alcotest.test_case "kuhn2 = kuhn under every algorithm" `Quick
      (matches_two_player (module Kuhn) (module (val Kuhn_n.game 2)) 300);
    Alcotest.test_case "leduc through the adapter" `Quick
      (matches_two_player (module Leduc) (module Multi.Of_game (Leduc)) 20);
    Alcotest.test_case "kuhn2 infoset keys" `Quick two_player_keys;
    Alcotest.test_case "three-player shape" `Quick shape;
    Alcotest.test_case "three-player payoffs" `Quick payoffs;
    qcheck "best response = brute force" best_response_is_best;
    qcheck "gains >= 0" gains_nonnegative;
    Alcotest.test_case "lands in the Szafron family" `Quick szafron_family;
    Alcotest.test_case "vanilla cfr lags" `Quick vanilla_is_slow;
    Alcotest.test_case "player count" `Quick rejects_sizes;
    Alcotest.test_case "deck size" `Quick ranks;
  ]
