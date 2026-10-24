open Cfr_solver

let count s = Range.size (Range.of_string s)

let combos () =
  Alcotest.(check int) "all" 1326 (Array.length Range.combos);
  Array.iteri
    (fun i c ->
      if Range.index c <> i then Alcotest.failf "index of combo %d" i;
      if Range.index (snd c, fst c) <> i then Alcotest.failf "swapped %d" i)
    Range.combos;
  Alcotest.(check string)
    "high card first" "AsKd"
    (Range.combo_to_string
       Range.combos.(Range.index (Hand.of_string "Kd", Hand.of_string "As")))

let notation () =
  Alcotest.(check int) "pair" 6 (count "QQ");
  Alcotest.(check int) "suited" 4 (count "AKs");
  Alcotest.(check int) "offsuit" 12 (count "AKo");
  Alcotest.(check int) "both" 16 (count "KA");
  Alcotest.(check int) "pairs up" 30 (count "TT+");
  Alcotest.(check int) "suited up" 20 (count "A9s+");
  Alcotest.(check int) "offsuit up" 24 (count "QTo+");
  Alcotest.(check int) "explicit" 1 (count "AhKh");
  Alcotest.(check int) "overlap counted once" 16 (count "AKs, AK");
  Alcotest.(check int) "random" 1326 (count "random");
  Alcotest.(check int) "empty" 0 (count "");
  List.iter
    (fun s ->
      Alcotest.check_raises s
        (Invalid_argument ("Range.of_string: " ^ s))
        (fun () -> ignore (Range.of_string s)))
    [ "AAs"; "AX"; "AhAh"; "AKx"; "AK:2"; "AhKh+" ]

let weights () =
  let r = Range.of_string "AA:0.5, AKs, AhKh:0.25" in
  Alcotest.(check int) "size" 10 (Range.size r);
  Alcotest.(check (float 1e-12)) "total" 6.25 (Range.total r);
  let ahkh = Range.index (Hand.of_string "Ah", Hand.of_string "Kh") in
  Array.iteri
    (fun i c ->
      if Range.index c = ahkh then
        Alcotest.(check (float 0.)) "last wins" 0.25 r.weights.(i))
    r.hands

let board () =
  let board = Hand.parse "As Kd 7h 4c 2s" in
  Alcotest.(check int) "random" 1081 (Range.size (Range.full ~board ()));
  Alcotest.(check int) "aces" 3 (Range.size (Range.of_string ~board "AA"));
  Alcotest.(check int) "ak" 9 (Range.size (Range.of_string ~board "AK"));
  Array.iter
    (fun c -> if Range.blocks board c then Alcotest.fail "board card in range")
    (Range.full ~board ()).hands;
  let sorted r =
    let ids = Array.map Range.index r.Range.hands in
    Array.for_all Fun.id
      (Array.init (Array.length ids - 1) (fun i -> ids.(i) < ids.(i + 1)))
  in
  Alcotest.(check bool)
    "sorted" true
    (sorted (Range.of_string ~board "AK, 22+, T9s"))

let tests =
  [
    Alcotest.test_case "combos" `Quick combos;
    Alcotest.test_case "notation" `Quick notation;
    Alcotest.test_case "weights" `Quick weights;
    Alcotest.test_case "board" `Quick board;
  ]
