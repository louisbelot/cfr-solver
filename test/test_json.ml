open Cfr_solver

let str = Alcotest.(check string)

let floats () =
  str "short" "0.1" (Json.to_string (Float 0.1));
  str "third" "0.3333333333333333" (Json.to_string (Float (1. /. 3.)));
  str "integral" "2" (Json.to_string (Float 2.));
  str "tiny" "1e-300" (Json.to_string (Float 1e-300));
  Alcotest.check_raises "nan" (Invalid_argument "Json: non-finite float")
    (fun () -> ignore (Json.to_string (Float Float.nan)))

let round_trip =
  QCheck2.Test.make ~count:1000 ~name:"floats round-trip"
    QCheck2.Gen.(float_range (-1e6) 1e6)
    (fun x -> float_of_string (Json.to_string (Float x)) = x)

let escaping () =
  str "quotes and control" {|"a\"b\\c\nd\u0001"|}
    (Json.to_string (String "a\"b\\c\nd\001"))

let layout () =
  let v =
    Json.Object
      [
        ("game", String "kuhn");
        ("empty", List []);
        ( "nested",
          Object [ ("J", Object [ ("p", Float 0.75); ("b", Float 0.25) ]) ] );
      ]
  in
  str "indented"
    "{\n\
    \  \"game\": \"kuhn\",\n\
    \  \"empty\": [],\n\
    \  \"nested\": {\n\
    \    \"J\": {\"p\": 0.75, \"b\": 0.25}\n\
    \  }\n\
     }"
    (Json.to_string v)

let strategy () =
  let s =
    Strategy.empty
    |> Strategy.add "Kb" ~actions:[| "p"; "b" |] ~probs:[| 0.; 1. |]
    |> Strategy.add "J" ~actions:[| "p"; "b" |] ~probs:[| 0.5; 0.5 |]
  in
  str "key order"
    "{\n  \"Kb\": {\"p\": 0, \"b\": 1},\n  \"J\": {\"p\": 0.5, \"b\": 0.5}\n}"
    (Json.to_string (Strategy.to_json ~keys:[ "Kb"; "missing"; "J" ] s))

let tests =
  [
    Alcotest.test_case "floats" `Quick floats;
    Alcotest.test_case "float round trip" `Quick (fun () ->
        QCheck2.Test.check_exn round_trip);
    Alcotest.test_case "escaping" `Quick escaping;
    Alcotest.test_case "layout" `Quick layout;
    Alcotest.test_case "strategy" `Quick strategy;
  ]
