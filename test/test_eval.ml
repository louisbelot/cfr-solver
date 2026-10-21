open Cfr_solver
module E = Eval.Make (Kuhn)

let constant ~p0 ~p1 =
  let fill player bet s =
    List.fold_left
      (fun s (key, actions) ->
        Strategy.add key ~actions ~probs:[| 1. -. bet; bet |] s)
      s (E.infosets player)
  in
  Strategy.empty |> fill P0 p0 |> fill P1 p1

let exact = Alcotest.float 1e-12

let pure_profiles () =
  Alcotest.check exact "always check" 0.
    (E.expected_value (constant ~p0:0. ~p1:0.));
  Alcotest.check exact "always bet" 0.
    (E.expected_value (constant ~p0:1. ~p1:1.));
  Alcotest.check exact "bet into a folder" 1.
    (E.expected_value (constant ~p0:1. ~p1:0.));
  Alcotest.check exact "fold to every bet" (-1.)
    (E.expected_value (constant ~p0:0. ~p1:1.))

let game_value () =
  List.iter
    (fun alpha ->
      Alcotest.check exact
        (Printf.sprintf "alpha = %g" alpha)
        (-1. /. 18.)
        (E.expected_value (Kuhn.equilibrium alpha)))
    [ 0.; 0.1; 0.2; 0.25; 1. /. 3. ]

let alpha_range () =
  Alcotest.check_raises "too big"
    (Invalid_argument "Kuhn.equilibrium: alpha must be in [0, 1/3]") (fun () ->
      ignore (Kuhn.equilibrium 0.5))

let infosets () =
  Alcotest.(check (list string))
    "player 0"
    [ "J"; "Jpb"; "Q"; "Qpb"; "K"; "Kpb" ]
    (List.map fst (E.infosets P0));
  Alcotest.(check int) "player 1" 6 (List.length (E.infosets P1))

let tests =
  [
    Alcotest.test_case "pure profiles" `Quick pure_profiles;
    Alcotest.test_case "equilibrium value is -1/18" `Quick game_value;
    Alcotest.test_case "alpha out of range" `Quick alpha_range;
    Alcotest.test_case "infosets" `Quick infosets;
  ]
