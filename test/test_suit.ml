open Cfr_solver

let deck = List.init 52 Fun.id

let perms () =
  Alcotest.(check int) "count" 24 (Array.length Suit.all);
  Alcotest.(check int)
    "distinct" 24
    (List.length (List.sort_uniq compare (Array.to_list Suit.all)));
  Array.iter
    (fun p ->
      if Suit.compose p (Suit.inverse p) <> Suit.identity then
        Alcotest.fail "inverse";
      let image = List.sort compare (List.map (Suit.apply p) deck) in
      if image <> deck then Alcotest.fail "not a bijection on cards";
      List.iter
        (fun c ->
          if Hand.rank (Suit.apply p c) <> Hand.rank c then
            Alcotest.fail "rank moved")
        deck)
    Suit.all;
  let p = Suit.all.(7) and q = Suit.all.(19) in
  List.iter
    (fun c ->
      if Suit.apply (Suit.compose p q) c <> Suit.apply p (Suit.apply q c) then
        Alcotest.fail "compose")
    deck

let classes board =
  let board = Array.to_list (Hand.parse board) in
  let group = Suit.stabilizer (Array.to_list Suit.all) board in
  let rest = List.filter (fun c -> not (List.mem c board)) deck in
  (List.length group, List.length (Suit.orbits group rest))

let boards () =
  let check name board group orbits =
    Alcotest.(check (pair int int)) name (group, orbits) (classes board)
  in
  check "rainbow" "Ks 9h 5d 2c" 1 48;
  check "two tone" "Ks 9s 5d 2d" 2 35;
  check "three suits" "Ks 9s 5d 2c" 1 48;
  check "monotone" "Ks 9s 5s 2s" 6 22;
  check "paired suits" "Ks Kh 7d 7c" 4 24;
  check "monotone flop" "Ks 9s 5s" 6 23

let orbit_members () =
  let board = Array.to_list (Hand.parse "Ks 9s 5s 2s") in
  let group = Suit.stabilizer (Array.to_list Suit.all) board in
  let rest = List.filter (fun c -> not (List.mem c board)) deck in
  let orbits = Suit.orbits group rest in
  Alcotest.(check int)
    "partition" 48
    (List.fold_left (fun n (_, m) -> n + List.length m) 0 orbits);
  List.iter
    (fun (c, members) ->
      if not (List.mem_assoc c members) then Alcotest.fail "rep not a member";
      List.iter
        (fun (d, p) ->
          if Suit.apply p c <> d then Alcotest.fail "perm does not map rep")
        members)
    orbits

let tests =
  [
    Alcotest.test_case "perms" `Quick perms;
    Alcotest.test_case "boards" `Quick boards;
    Alcotest.test_case "orbit members" `Quick orbit_members;
  ]
