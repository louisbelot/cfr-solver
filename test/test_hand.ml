open Cfr_solver

let ev s = Hand.eval (Hand.parse s)

let names () =
  for c = 0 to 51 do
    Alcotest.(check int) "round trip" c (Hand.of_string (Hand.to_string c))
  done;
  Alcotest.(check string) "ten" "Td" (Hand.to_string (Hand.of_string "td"));
  Alcotest.check_raises "bad rank" (Invalid_argument "Hand.of_string: 1s")
    (fun () -> ignore (Hand.of_string "1s"));
  Alcotest.check_raises "empty" (Invalid_argument "Hand.of_string: ") (fun () ->
      ignore (Hand.of_string ""))

let same name a b = Alcotest.(check int) name (ev b) (ev a)

let better name a b =
  if not (ev a > ev b) then Alcotest.failf "%s: %s should beat %s" name a b

let by_hand () =
  better "six-high straight beats the wheel" "2c 3d 4s 5h 6c" "Ah 2c 3d 4s 5h";
  better "wheel beats trips" "Ah 2c 3d 4s 5h" "Kc Kd Kh 2s 3c";
  better "royal beats quads" "Ah Kh Qh Jh Th" "9s 9h 9d 9c Ah";
  better "steel wheel is the lowest straight flush" "2d 3d 4d 5d 6d"
    "Ad 2d 3d 4d 5d";
  better "flush kicker" "Ah 9h 7h 4h 2h" "Kh Qh Jh 9h 7h";
  better "two pair kicker" "Ah Ad 9c 9s 5h" "Ac As 9d 9h 4c";
  better "full house by trips" "3c 3d 3h 2s 2c" "2h 2d 2s Ac Ad";
  same "best flush of six" "As Ks 9s 7s 2s Qs 3d" "As Ks Qs 9s 7s";
  same "three pairs" "Ah Ad 9c 9s 4h 4d Kc" "Ah Ad 9c 9s Kc";
  same "third pair as kicker" "Ah Ad 9c 9s 4h 4d 2c" "Ah Ad 9c 9s 4h";
  same "two trips" "7c 7d 7h 3s 3c 3d Ks" "7c 7d 7h 3s 3c";
  same "quads and trips" "5c 5d 5h 5s Kc Kd Kh" "5c 5d 5h 5s Kc";
  same "straight over a pair" "6c 7d 8h 9s Tc Td 2h" "6c 7d 8h 9s Tc";
  same "flush over a straight" "2h 4h 6h 8h Th 7c 9d" "2h 4h 6h 8h Th";
  same "wheel among broadway cards" "Ah 2c 3d 4s 5h Kc Qd" "Ah 2c 3d 4s 5h";
  same "six to the ace" "9c Tc Jd Qh Ks Ad 2c" "Tc Jd Qh Ks Ad";
  Alcotest.(check string)
    "category" "straight flush"
    (Hand.category_name (Hand.category (ev "Ad 2d 3d 4d 5d")))

let five_card_counts () =
  let counts = Array.make 9 0 and seen = Hashtbl.create 8192 in
  let h = Array.make 5 0 in
  for a = 0 to 51 do
    h.(0) <- a;
    for b = a + 1 to 51 do
      h.(1) <- b;
      for c = b + 1 to 51 do
        h.(2) <- c;
        for d = c + 1 to 51 do
          h.(3) <- d;
          for e = d + 1 to 51 do
            h.(4) <- e;
            let v = Hand.eval h in
            let k = v lsr 20 in
            counts.(k) <- counts.(k) + 1;
            Hashtbl.replace seen v ()
          done
        done
      done
    done
  done;
  Alcotest.(check (array int))
    "categories"
    [| 1302540; 1098240; 123552; 54912; 10200; 5108; 3744; 624; 40 |]
    counts;
  Alcotest.(check int) "distinct values" 7462 (Hashtbl.length seen)

let seven =
  QCheck2.Gen.(
    map
      (fun keys ->
        let deck = Array.init 52 (fun c -> (List.nth keys c, c)) in
        Array.sort compare deck;
        Array.init 7 (fun i -> snd deck.(i)))
      (list_size (return 52) int))

let best_of_five cards =
  let best = ref 0 in
  for skip1 = 0 to 6 do
    for skip2 = skip1 + 1 to 6 do
      let five =
        Array.of_list
          (List.filteri
             (fun i _ -> i <> skip1 && i <> skip2)
             (Array.to_list cards))
      in
      best := max !best (Hand.eval five)
    done
  done;
  !best

let subsets () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:2000 ~name:"best five of seven" seven
       (fun cards -> Hand.eval cards = best_of_five cards))

let rejects () =
  Alcotest.check_raises "four cards"
    (Invalid_argument "Hand.eval: 5 to 7 cards") (fun () ->
      ignore (ev "Ah Kh Qh Jh"))

let tests =
  [
    Alcotest.test_case "card names" `Quick names;
    Alcotest.test_case "hands by hand" `Quick by_hand;
    Alcotest.test_case "all five-card hands" `Quick five_card_counts;
    Alcotest.test_case "seven cards are the best five" `Quick subsets;
    Alcotest.test_case "arguments" `Quick rejects;
  ]
