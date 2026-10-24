open Cfr_solver
open Bet_tree

let rec lines path = function
  | Decision { actions; children; _ } ->
      List.concat
        (List.mapi
           (fun a child -> lines (path ^ action_to_string actions.(a)) child)
           (Array.to_list children))
  | Chance child -> lines (path ^ "/") child
  | leaf -> [ (path, leaf) ]

let leaf_value = function
  | Folded { folder; win } -> Printf.sprintf "fold%d %g" folder win
  | Showdown { win } -> Printf.sprintf "show %g" win
  | _ -> "?"

let rec find path node =
  match (path, node) with
  | [], _ -> node
  | "/" :: rest, Chance child -> find rest child
  | a :: rest, Decision { actions; children; _ } ->
      let rec go i =
        if i = Array.length actions then Alcotest.failf "no action %s" a
        else if action_to_string actions.(i) = a then find rest children.(i)
        else go (i + 1)
      in
      go 0
  | _ -> Alcotest.fail "bad path"

let actions_at path node =
  match find path node with
  | Decision { actions; _ } ->
      String.concat " " (Array.to_list (Array.map action_to_string actions))
  | _ -> Alcotest.fail "not a decision"

let limit_river () =
  let t = build (limit ~pot:4. ~bet:1. 1) in
  Alcotest.(check int) "decisions" 10 (count t);
  let got = List.map (fun (p, l) -> (p, leaf_value l)) (lines "" t) in
  Alcotest.(check (list (pair string string)))
    "lines"
    [
      ("xx", "show 2");
      ("xb1f", "fold0 2");
      ("xb1c", "show 3");
      ("xb1b2f", "fold1 3");
      ("xb1b2c", "show 4");
      ("xb1b2b3f", "fold0 4");
      ("xb1b2b3c", "show 5");
      ("xb1b2b3b4f", "fold1 5");
      ("xb1b2b3b4c", "show 6");
      ("b1f", "fold1 2");
      ("b1c", "show 3");
      ("b1b2f", "fold0 3");
      ("b1b2c", "show 4");
      ("b1b2b3f", "fold1 4");
      ("b1b2b3c", "show 5");
      ("b1b2b3b4f", "fold0 5");
      ("b1b2b3b4c", "show 6");
    ]
    got

let limit_turn () =
  let t = build (limit ~pot:4. ~bet:1. 2) in
  Alcotest.(check int) "decisions" (10 + (9 * 10)) (count t);
  Alcotest.(check string)
    "river pot" "show 5"
    (leaf_value (find [ "b1"; "c"; "/"; "b1"; "b2"; "c" ] t));
  Alcotest.(check string)
    "fold on the river" "fold0 5"
    (leaf_value (find [ "x"; "b1"; "b2"; "c"; "/"; "b1"; "b2"; "f" ] t))

let no_limit () =
  let street =
    { bets = [ Pot 0.5; Pot 1.; All_in ]; raises = [ Pot 1.; All_in ]; cap = 3 }
  in
  let t = build { pot = 10.; stack = 35.; streets = [ street ] } in
  Alcotest.(check string) "root" "x b5 b10 b35" (actions_at [] t);
  Alcotest.(check string) "pot raise" "f c b25 b35" (actions_at [ "b5" ] t);
  Alcotest.(check string) "clamped" "f c b35" (actions_at [ "b10" ] t);
  Alcotest.(check string) "facing all-in" "f c" (actions_at [ "b35" ] t);
  Alcotest.(check string)
    "call all-in" "show 40"
    (leaf_value (find [ "b5"; "b35"; "c" ] t))

let fractions () =
  let t =
    build
      {
        pot = 10.;
        stack = 100.;
        streets = [ { bets = [ Pot 0.75 ]; raises = [ Pot 1. ]; cap = 2 } ];
      }
  in
  let check_fractions path expected =
    match find path t with
    | Decision { pot; committed; to_call; actions; _ } ->
        let fs =
          Array.to_list actions
          |> List.filter_map (function
            | Bet _ as a -> Some (fraction ~pot ~committed ~to_call a)
            | _ -> None)
        in
        Alcotest.(check (list (float 1e-12)))
          (String.concat "" path) expected fs
    | _ -> Alcotest.fail "not a decision"
  in
  check_fractions [] [ 0.75 ];
  check_fractions [ "x" ] [ 0.75 ];
  check_fractions [ "b7.5" ] [ 1. ];
  check_fractions [ "x"; "b7.5"; "b32.5" ] []

let all_in_runout () =
  let s = { bets = [ All_in ]; raises = []; cap = 1 } in
  let t = build { pot = 2.; stack = 3.; streets = [ s; s ] } in
  (match find [ "b3"; "c" ] t with
  | Chance (Showdown { win = 4. }) -> ()
  | _ -> Alcotest.fail "all-in call should run out to showdown");
  Alcotest.(check string) "checked down" "x b3" (actions_at [ "x"; "x"; "/" ] t)

let invalid () =
  let s = { bets = [ Pot 1. ]; raises = []; cap = 1 } in
  List.iter
    (fun (name, cfg) ->
      match build cfg with
      | _ -> Alcotest.failf "%s accepted" name
      | exception Invalid_argument _ -> ())
    [
      ("zero pot", { pot = 0.; stack = 1.; streets = [ s ] });
      ("no streets", { pot = 1.; stack = 1.; streets = [] });
      ( "negative size",
        { pot = 1.; stack = 1.; streets = [ { s with bets = [ Pot (-1.) ] } ] }
      );
      ( "negative cap",
        { pot = 1.; stack = 1.; streets = [ { s with cap = -1 } ] } );
    ]

let zero_sum_bounds () =
  let street =
    {
      bets = [ Pot 0.33; Pot 1.5; All_in ];
      raises = [ Pot 0.7; All_in ];
      cap = 4;
    }
  in
  let cfg = { pot = 6.; stack = 20.; streets = [ street; street ] } in
  List.iter
    (fun (path, leaf) ->
      match leaf with
      | Folded { win; _ } | Showdown { win } ->
          if win < 3. || win > 23. +. 1e-9 then
            Alcotest.failf "%s wins %g" path win
      | _ -> ())
    (lines "" (build cfg))

let tests =
  [
    Alcotest.test_case "limit river" `Quick limit_river;
    Alcotest.test_case "limit turn" `Quick limit_turn;
    Alcotest.test_case "no limit" `Quick no_limit;
    Alcotest.test_case "fractions" `Quick fractions;
    Alcotest.test_case "all-in runout" `Quick all_in_runout;
    Alcotest.test_case "invalid" `Quick invalid;
    Alcotest.test_case "bounds" `Quick zero_sum_bounds;
  ]
