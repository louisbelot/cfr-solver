open Cfr_solver

let rat = Alcotest.testable Rat.pp Rat.equal
let zero = Alcotest.(pair rat rat)

let config ?(rule = Auction.First_price) ?(ties = Auction.Split) ~bids prior
    value =
  { Bidding.bids; rule; ties; prior; value }

let certify c f =
  let t = Lemke.build (module (val Bidding.game c)) in
  let strat =
    List.fold_left
      (fun acc i ->
        List.fold_left
          (fun acc s ->
            Strategy.add (Bidding.key i s)
              ~actions:(Array.init (c.bids + 1) string_of_int)
              ~probs:(Array.map Rat.to_float (f i s))
              acc)
          acc (Bidding.signals c i))
      Strategy.empty [ 0; 1 ]
  in
  Lemke.gains t (Lemke.plan t 0 strat, Lemke.plan t 1 strat)

let matches_auction () =
  let ac =
    {
      Auction.bidders = 2;
      top = 6;
      rule = First_price;
      ties = Split;
      reserve = 0;
    }
  in
  let c = config ~bids:6 (Bidding.independent 6 6) Bidding.private_value in
  let _, sol, sigma = Bidding.equilibrium c in
  let profile = Array.init 2 (fun i -> Array.init 7 (sigma i)) in
  Alcotest.check rat "auction nash conv" Rat.zero
    (Auction.Exact.nash_conv ac profile);
  let u0, u1, _ = Bidding.outcome c sigma in
  Alcotest.check zero "values" sol.values (u0, u1)

let truthful_second_price () =
  let c =
    config ~rule:Second_price ~bids:8 (Bidding.independent 8 4)
      Bidding.private_value
  in
  Alcotest.check zero "truthful" (Rat.zero, Rat.zero)
    (certify c (Bidding.pure c (fun i s -> Bidding.naive c i s)))

let wallet_second_price () =
  let c =
    config ~rule:Second_price ~bids:12 (Bidding.independent 6 6) Bidding.wallet
  in
  Alcotest.check zero "2s" (Rat.zero, Rat.zero)
    (certify c (Bidding.pure c (fun _ s -> 2 * s)));
  let g0, g1 = certify c (Bidding.pure c (Bidding.naive c)) in
  Alcotest.(check bool) "naive gains" true (Rat.sign g0 > 0 && Rat.sign g1 > 0)

let asymmetric () =
  List.iter
    (fun rule ->
      let c =
        config ~rule ~bids:8 (Bidding.independent 8 4) Bidding.private_value
      in
      let t, sol, sigma = Bidding.equilibrium c in
      Alcotest.check zero "gains" (Rat.zero, Rat.zero) (Lemke.gains t sol.plans);
      let u0, u1, _ = Bidding.outcome c sigma in
      Alcotest.check zero "values" sol.values (u0, u1))
    [ Auction.First_price; All_pay ]

let affiliation () =
  let x = 6 in
  let f = Hashtbl.of_seq (List.to_seq (Bidding.affiliated x)) in
  Alcotest.check rat "mass" Rat.one
    (Hashtbl.fold (fun _ p acc -> Rat.add acc p) f Rat.zero);
  for a = 0 to x do
    for a' = a to x do
      for b = 0 to x do
        for b' = b to x do
          let g s = Hashtbl.find f s in
          Alcotest.(check bool)
            "log-supermodular" true
            (Rat.compare
               (Rat.mul (g (a', b')) (g (a, b)))
               (Rat.mul (g (a', b)) (g (a, b')))
            >= 0)
        done
      done
    done
  done

let tests =
  [
    Alcotest.test_case "same equilibrium as the auction module" `Quick
      matches_auction;
    Alcotest.test_case "truthful second price, asymmetric" `Quick
      truthful_second_price;
    Alcotest.test_case "wallet game bids 2s" `Quick wallet_second_price;
    Alcotest.test_case "asymmetric equilibria" `Quick asymmetric;
    Alcotest.test_case "affiliated prior" `Quick affiliation;
  ]
