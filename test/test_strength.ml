open Cfr_solver

let d = Leduc.standard
let close = Alcotest.(check (float 1e-12))

let by_hand () =
  close "king on a king" 1. (Strength.hs d 2 2);
  close "jack on a queen" 0.125 (Strength.hs d 0 1);
  close "king on a jack" 0.625 (Strength.hs d 2 0);
  close "ehs jack" 0.3 (Strength.ehs d 0);
  close "ehs queen" 0.5 (Strength.ehs d 1);
  close "ehs king" 0.7 (Strength.ehs d 2);
  close "ehs2 king" ((0.8 *. 0.625 *. 0.625) +. 0.2) (Strength.ehs2 d 2)

let decks =
  QCheck2.Gen.(
    let* ranks = int_range 1 13 and* suits = int_range 1 4 in
    return { Leduc.ranks; suits })
  |> QCheck2.Gen.map (fun (d : Leduc.deck) ->
      if Leduc.cards d < 3 then { d with suits = 3 } else d)

let fair (d : Leduc.deck) =
  let total = ref 0. in
  for c = 0 to d.ranks - 1 do
    total := !total +. Strength.ehs d c
  done;
  Float.abs ((!total /. float_of_int d.ranks) -. 0.5) < 1e-12

let fair_property () =
  QCheck2.Test.check_exn (QCheck2.Test.make ~count:100 ~name:"fair" decks fair)

let checked d path =
  let module G = (val Leduc.game d) in
  let module C = Game_check.Make (G) in
  let rec go h =
    match G.node h with
    | Game.Terminal u -> u
    | Chance outcomes ->
        List.fold_left (fun acc (p, h') -> acc +. (p *. go h')) 0. outcomes
    | Decision { actions; _ } -> go (List.assoc "c" actions)
  in
  go (C.follow G.root path)

let matches_showdowns () =
  List.iter
    (fun (d : Leduc.deck) ->
      for c = 0 to d.ranks - 1 do
        close
          (Printf.sprintf "%dx%d card %d" d.ranks d.suits c)
          (checked d [ string_of_int c ])
          ((2. *. Strength.ehs d c) -. 1.)
      done)
    [
      d;
      { ranks = 5; suits = 2 };
      { ranks = 4; suits = 3 };
      { ranks = 6; suits = 1 };
    ]

let histograms () =
  let d = { Leduc.ranks = 13; suits = 2 } in
  for c = 0 to 12 do
    let h = Strength.histogram ~bins:20 d c in
    close "mass" 1. (Array.fold_left ( +. ) 0. h);
    close "pair bin" (1. /. 25.) h.(19)
  done

let round1_buckets () =
  let d = { Leduc.ranks = 13; suits = 2 } in
  List.iter
    (fun f ->
      let b = Buckets.round1 d f 4 in
      Alcotest.(check int) "four" 4 (1 + Array.fold_left max 0 b.labels);
      for c = 1 to 12 do
        if b.labels.(c) < b.labels.(c - 1) then
          Alcotest.failf "%s: bucket order breaks at %d" b.name c
      done)
    Buckets.[ Ehs; Ehs2; Hist 20 ];
  let exact = Buckets.round1 d Ehs 13 in
  Alcotest.(check (array int))
    "one per rank" (Array.init 13 Fun.id) exact.labels

let lossless () =
  let round1 = Buckets.round1 d Ehs 3 in
  let abstraction = Buckets.abstraction ~round1 d in
  let module G = (val Leduc.game ~abstraction d) in
  let module A = Abstraction.Make (Leduc) (G) in
  Alcotest.(check int) "size" 288 A.size;
  let round2 = Buckets.round2 d 1 in
  let abstraction = Buckets.abstraction ~round1 ~round2 ~recall:false d in
  let module G = (val Leduc.game ~abstraction d) in
  let module A = Abstraction.Make (Leduc) (G) in
  Alcotest.(check int) "forgetful" (18 + 30) A.size;
  Alcotest.(check string) "name" "ehs3/hs1 imperfect" abstraction.name

let tests =
  [
    Alcotest.test_case "values by hand" `Quick by_hand;
    Alcotest.test_case "mean strength is a half" `Quick fair_property;
    Alcotest.test_case "matches check-down payoffs" `Quick matches_showdowns;
    Alcotest.test_case "histograms" `Quick histograms;
    Alcotest.test_case "round 1 buckets" `Quick round1_buckets;
    Alcotest.test_case "abstraction sizes" `Quick lossless;
  ]
