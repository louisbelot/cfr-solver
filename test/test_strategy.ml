open Cfr_solver

let two = [| "p"; "b" |]

let missing_is_uniform () =
  Alcotest.(check (array (float 0.)))
    "uniform"
    [| 1. /. 3.; 1. /. 3.; 1. /. 3. |]
    (Strategy.probs Strategy.empty "K" ~n:3)

let wrong_arity () =
  let s = Strategy.add "K" ~actions:two ~probs:[| 0.; 1. |] Strategy.empty in
  Alcotest.check_raises "arity"
    (Invalid_argument "Strategy.probs: wrong action count at K") (fun () ->
      ignore (Strategy.probs s "K" ~n:3))

let union_prefers_left () =
  let mk p =
    Strategy.add "K" ~actions:two ~probs:[| p; 1. -. p |] Strategy.empty
  in
  let s = Strategy.union (mk 0.25) (mk 0.75) in
  Alcotest.(check (float 0.)) "left" 0.25 (Strategy.probs s "K" ~n:2).(0)

let table () =
  let s =
    Strategy.empty
    |> Strategy.add "Kpb" ~actions:two ~probs:[| 0.; 1. |]
    |> Strategy.add "J" ~actions:two ~probs:[| 0.75; 0.25 |]
  in
  Alcotest.(check string)
    "table"
    "  J    p 0.750  b 0.250\n  Kpb  p 0.000  b 1.000\n  Q    (uniform)\n"
    (Format.asprintf "%a" (Strategy.pp ~keys:[ "J"; "Kpb"; "Q" ]) s)

let tests =
  [
    Alcotest.test_case "missing infoset is uniform" `Quick missing_is_uniform;
    Alcotest.test_case "wrong action count" `Quick wrong_arity;
    Alcotest.test_case "union" `Quick union_prefers_left;
    Alcotest.test_case "table" `Quick table;
  ]
