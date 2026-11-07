open Cfr_solver
module L3 = (val Leduc_n.game 3)

let rank_index dealt r =
  let left q = 2 - List.length (List.filter (( = ) q) dealt) in
  List.length (List.filter (fun q -> q < r && left q > 0) (List.init 4 Fun.id))

let follow cards moves =
  let rec go h dealt cards moves =
    match (L3.node h, cards, moves) with
    | Multi.Chance outcomes, r :: cards, _ ->
        go
          (snd (List.nth outcomes (rank_index dealt r)))
          (r :: dealt) cards moves
    | Decision { actions; _ }, _, m :: moves ->
        go (List.assoc m actions) dealt cards moves
    | node, [], [] -> node
    | _ -> Alcotest.fail "path does not fit the tree"
  in
  let split s = List.init (String.length s) (fun i -> String.make 1 s.[i]) in
  go L3.root [] cards (split moves)

let payoff cards moves =
  match follow cards moves with
  | Multi.Terminal u -> u
  | _ -> Alcotest.fail "not terminal"

let payoffs () =
  let check name expected cards moves =
    Alcotest.(check (array (float 0.))) name expected (payoff cards moves)
  in
  check "checked down" [| -1.; -1.; 2. |] [ 0; 1; 2; 3 ] "cccccc";
  check "raise, two folds" [| 2.; -1.; -1. |] [ 0; 1; 2 ] "rff";
  check "caller after a raise" [| 6.; -3.; -3. |] [ 3; 1; 2; 0 ] "crccccc";
  check "pair on the board" [| 6.; -5.; -1. |] [ 0; 1; 2; 0 ] "cccrcf";
  check "split pot" [| 0.5; 0.5; -1. |] [ 1; 1; 0; 2 ] "cccrcf";
  check "folded player skipped" [| -3.; -1.; 4. |] [ 0; 1; 2; 3 ] "rfccc";
  check "folds on the turn" [| 2.; -1.; -1. |] [ 0; 1; 2; 3 ] "cccrff"

let decisions () =
  let at cards moves =
    match follow cards moves with
    | Multi.Decision { player; infoset; actions } ->
        (player, infoset, List.map fst actions)
    | _ -> Alcotest.fail "not a decision"
  in
  let check name expected got =
    Alcotest.(check (triple int string (list string))) name expected got
  in
  check "opening" (0, "T:", [ "c"; "r" ]) (at [ 0; 1; 2 ] "");
  check "raise cap" (2, "Q:rr", [ "f"; "c" ]) (at [ 0; 1; 2 ] "rr");
  check "called raiser acts again"
    (0, "T:crr", [ "f"; "c" ])
    (at [ 0; 1; 2 ] "crr");
  check "second round skips the fold"
    (2, "QK:rfc/c", [ "c"; "r" ])
    (at [ 0; 1; 2; 3 ] "rfcc")

let suited_sizes players () =
  let module G = (val Leduc_n.game ~ranks:3 players) in
  let module E = Multi.Eval (G) in
  let infosets =
    List.init players Fun.id |> List.concat_map E.infosets
    |> List.fold_left
         (fun acc (key, _) ->
           match String.index_opt key ':' with
           | Some 1 -> acc + 2
           | _ -> acc + if key.[0] = key.[1] then 2 else 4)
         0
  in
  let histories = ref 0. in
  let rec go h dealt m =
    if dealt >= players then histories := !histories +. m;
    match G.node h with
    | Multi.Terminal u ->
        Alcotest.(check (float 1e-12))
          "zero-sum" 0.
          (Array.fold_left ( +. ) 0. u)
    | Chance outcomes ->
        let left = float_of_int (6 - dealt) in
        List.iter
          (fun (p, h') -> go h' (dealt + 1) (m *. Float.round (p *. left)))
          outcomes
    | Decision { actions; _ } ->
        List.iter (fun (_, h') -> go h' dealt m) actions
  in
  go G.root 0 1.;
  let expected_h, expected_i =
    if players = 2 then (9450, 936) else (396120, 13878)
  in
  Alcotest.(check int) "suited infosets" expected_i infosets;
  Alcotest.(check int) "suited histories" expected_h (Float.to_int !histories)

let rejects () =
  Alcotest.check_raises "deck" (Invalid_argument "Leduc_n.game: deck too small")
    (fun () -> ignore (Leduc_n.game ~ranks:1 3))

let tests =
  [
    Alcotest.test_case "leduc2 = leduc under every algorithm" `Quick
      (Test_multi.matches_two_player
         (module Leduc)
         (module (val Leduc_n.game 2))
         20);
    Alcotest.test_case "three-player payoffs" `Quick payoffs;
    Alcotest.test_case "three-player decisions" `Quick decisions;
    Alcotest.test_case "two players, published sizes" `Quick (suited_sizes 2);
    Alcotest.test_case "three players, published sizes" `Quick (suited_sizes 3);
    Alcotest.test_case "deck size" `Quick rejects;
  ]
