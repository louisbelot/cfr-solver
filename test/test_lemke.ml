open Cfr_solver

let rat = Alcotest.testable Rat.pp Rat.equal

let zero_gains t sol =
  Alcotest.(check (pair rat rat))
    "gains" (Rat.zero, Rat.zero)
    (Lemke.gains t sol.Lemke.plans)

module Two_by_two (U : sig
  val u : string -> float array
end) =
struct
  type history = string

  let name = "2x2"
  let players = 2
  let root = ""
  let moves h = [ ("A", h ^ "A"); ("B", h ^ "B") ]

  let node = function
    | "" -> Multi.Decision { player = 0; infoset = "0"; actions = moves "" }
    | ("A" | "B") as h ->
        Decision { player = 1; infoset = "1"; actions = moves h }
    | h -> Terminal (U.u h)
end

module Bos = Two_by_two (struct
  let u = function
    | "AA" -> [| 2.; 1. |]
    | "BB" -> [| 1.; 2. |]
    | _ -> [| 0.; 0. |]
end)

module Cycle = Two_by_two (struct
  let u = function
    | "AA" -> [| 3.; 0. |]
    | "AB" -> [| 0.; 1. |]
    | "BA" -> [| 0.; 2. |]
    | _ -> [| 1.; 0. |]
end)

let kuhn () =
  let t = Lemke.build (module Multi.Of_game (Kuhn)) in
  let sol = Lemke.solve t in
  Alcotest.(check (pair rat rat))
    "values"
    (Rat.of_ints (-1) 18, Rat.of_ints 1 18)
    sol.values;
  zero_gains t sol

let leduc2 () =
  let module L = (val Leduc.game { Leduc.ranks = 2; suits = 2 }) in
  let t = Lemke.build (module Multi.Of_game (L)) in
  let sol = Lemke.solve t in
  Alcotest.check rat "value" Rat.zero (fst sol.values);
  zero_gains t sol

let bos () =
  let t = Lemke.build (module Bos) in
  let sol = Lemke.solve t in
  zero_gains t sol;
  let pure = [ (Rat.of_int 2, Rat.of_int 1); (Rat.of_int 1, Rat.of_int 2) ] in
  Alcotest.(check bool)
    "pure" true
    (List.exists
       (fun (u, v) ->
         Rat.equal u (fst sol.values) && Rat.equal v (snd sol.values))
       pure)

let mixed () =
  let t = Lemke.build (module Cycle) in
  let sol = Lemke.solve t in
  zero_gains t sol;
  let x, y = sol.plans in
  Alcotest.(check (list (pair string (array rat))))
    "behavior"
    [
      ("0", [| Rat.of_ints 2 3; Rat.of_ints 1 3 |]);
      ("1", [| Rat.of_ints 1 4; Rat.of_ints 3 4 |]);
    ]
    (Lemke.behavior t 0 x @ Lemke.behavior t 1 y);
  Alcotest.(check (pair rat rat))
    "values"
    (Rat.of_ints 3 4, Rat.of_ints 2 3)
    sol.values

let bos_prior () =
  let t = Lemke.build (module Bos) in
  let lean p q =
    Strategy.empty
    |> Strategy.add "0" ~actions:[| "A"; "B" |] ~probs:[| p; 1. -. p |]
    |> Strategy.add "1" ~actions:[| "A"; "B" |] ~probs:[| q; 1. -. q |]
  in
  let pure p q = Lemke.solve ~prior:(lean p q) t in
  let a = pure 0.9 0.9 and b = pure 0.1 0.1 in
  zero_gains t a;
  zero_gains t b;
  Alcotest.(check (pair rat rat)) "AA" (Rat.of_int 2, Rat.of_int 1) a.values;
  Alcotest.(check (pair rat rat)) "BB" (Rat.of_int 1, Rat.of_int 2) b.values

let auction ties () =
  List.iter
    (fun top ->
      let c =
        { Auction.bidders = 2; top; rule = First_price; ties; reserve = 0 }
      in
      let t = Lemke.build (module (val Auction.game c)) in
      let sol = Lemke.solve t in
      zero_gains t sol;
      let profile =
        Array.init 2 (fun i ->
            let plan = if i = 0 then fst sol.plans else snd sol.plans in
            let rows = Lemke.behavior t i plan in
            Array.init (top + 1) (fun v -> List.assoc (Auction.key i v) rows))
      in
      Alcotest.check rat
        (Printf.sprintf "nash conv x=%d" top)
        Rat.zero
        (Auction.Exact.nash_conv c profile))
    [ 2; 3; 4; 5; 6 ]

let split_ten () =
  let c =
    {
      Auction.bidders = 2;
      top = 10;
      rule = First_price;
      ties = Split;
      reserve = 0;
    }
  in
  let t = Lemke.build (module (val Auction.game c)) in
  let x, y = (Lemke.solve t).plans in
  let row p v =
    List.assoc (Auction.key p v) (Lemke.behavior t p (if p = 0 then x else y))
  in
  let mix b q =
    Array.init 11 (fun k ->
        if k = b then q else if k = b + 1 then Rat.sub Rat.one q else Rat.zero)
  in
  List.iter
    (fun p ->
      Alcotest.(check (array rat)) "v=7" (mix 3 (Rat.of_ints 8 19)) (row p 7);
      Alcotest.(check (array rat)) "v=10" (mix 4 (Rat.of_ints 9 19)) (row p 10))
    [ 0; 1 ]

module Random_game (P : sig
  val seed : int
  val depth : int
end) =
struct
  type history = (int * int) list

  let name = "random"
  let players = 2
  let root = []
  let hash x = Hashtbl.hash (P.seed, x)
  let kind d = if d = 0 then 2 else hash ("kind", d) mod 3
  let seen d p = d = 0 || hash ("seen", d, p) mod 2 = 0

  let view p h =
    List.rev h
    |> List.map (fun (d, a) ->
        if kind d = p || seen d p then Printf.sprintf "%d:%d" d a
        else Printf.sprintf "%d:?" d)
    |> String.concat " "

  let node h =
    let d = List.length h in
    if d = P.depth then
      Multi.Terminal
        (Array.init 2 (fun p -> float_of_int ((hash ("u", p, h) mod 11) - 5)))
    else
      match kind d with
      | 2 ->
          let n = 2 + (hash ("n", d) mod 2) in
          let w = List.init n (fun a -> 1 + a) in
          let total = List.fold_left ( + ) 0 w in
          Chance
            (List.mapi
               (fun a k -> (float_of_int k /. float_of_int total, (d, a) :: h))
               w)
      | p ->
          let key = Printf.sprintf "%d|%s" d (view p h) in
          let n = 2 + (hash ("n", key) mod 2) in
          Decision
            {
              player = p;
              infoset = key;
              actions = List.init n (fun a -> (string_of_int a, (d, a) :: h));
            }
end

let valid t p x =
  List.for_all
    (fun (_, probs) ->
      Array.for_all (fun q -> Rat.sign q >= 0) probs
      && Rat.equal Rat.one (Array.fold_left Rat.add Rat.zero probs))
    (Lemke.behavior t p x)
  && Rat.equal x.(0) Rat.one

let random_games () =
  QCheck2.Test.check_exn
  @@ QCheck2.Test.make ~name:"random general-sum games" ~count:60
       QCheck2.Gen.(pair (int_bound 1_000_000) (int_range 2 5))
       (fun (seed, depth) ->
         let module G = Random_game (struct
           let seed = seed
           let depth = depth
         end) in
         let t = Lemke.build (module G) in
         let sol = Lemke.solve t in
         let x, y = sol.plans in
         let g0, g1 = Lemke.gains t sol.plans in
         valid t 0 x && valid t 1 y && Rat.is_zero g0 && Rat.is_zero g1)

let tests =
  [
    Alcotest.test_case "kuhn value" `Quick kuhn;
    Alcotest.test_case "leduc two ranks" `Quick leduc2;
    Alcotest.test_case "battle of the sexes" `Quick bos;
    Alcotest.test_case "unique mixed equilibrium" `Quick mixed;
    Alcotest.test_case "prior picks the equilibrium" `Quick bos_prior;
    Alcotest.test_case "first price, void ties" `Quick (auction Auction.Void);
    Alcotest.test_case "first price, split ties" `Quick (auction Auction.Split);
    Alcotest.test_case "split ties at x = 10" `Quick split_ten;
    Alcotest.test_case "random general-sum games" `Quick random_games;
  ]
