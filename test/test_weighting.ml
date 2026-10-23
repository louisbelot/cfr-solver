open Cfr_solver

let table () = Table.create (Tree.build (module Kuhn))

let telescopes () =
  QCheck2.Test.check_exn
  @@ QCheck2.Test.make ~count:200 ~name:"discounting is weighting by t^gamma"
       QCheck2.Gen.(pair (int_range 1 30) (int_bound 10_000))
       (fun (n, seed) ->
         let rng = Random.State.make [| seed |] in
         let lin = table () and dcfr = table () in
         let slots = Table.slots lin in
         let regret = Array.make slots 0. and sum1 = Array.make slots 0. in
         let sum2 = Array.make slots 0. in
         for t = 1 to n do
           let w = float_of_int t in
           for k = 0 to slots - 1 do
             let x = Random.State.float rng 2. -. 1.
             and s = Random.State.float rng 1. in
             lin.regret.(k) <- lin.regret.(k) +. x;
             lin.strategy_sum.(k) <- lin.strategy_sum.(k) +. s;
             dcfr.strategy_sum.(k) <- dcfr.strategy_sum.(k) +. s;
             regret.(k) <- regret.(k) +. (w *. x);
             sum1.(k) <- sum1.(k) +. (w *. s);
             sum2.(k) <- sum2.(k) +. (w *. w *. s)
           done;
           Table.discount lin (Cfr.discounts Linear t);
           Table.discount dcfr (Cfr.discounts Cfr.dcfr t)
         done;
         let m = float_of_int (n + 1) in
         let close a b =
           Float.abs (a -. b) <= 1e-9 *. Float.max 1. (Float.abs b)
         in
         List.for_all Fun.id
           (List.init slots (fun k ->
                close lin.regret.(k) (regret.(k) /. m)
                && close lin.strategy_sum.(k) (sum1.(k) /. m)
                && close dcfr.strategy_sum.(k) (sum2.(k) /. (m *. m)))))

let plus_floors () =
  let t = table () in
  Array.iteri (fun k _ -> t.regret.(k) <- float_of_int (k - 10)) t.regret;
  Table.discount t (Cfr.discounts Plus 3);
  Array.iteri
    (fun k r ->
      Alcotest.(check (float 0.))
        (string_of_int k)
        (Float.max 0. (float_of_int (k - 10)))
        r)
    t.regret

module K = Mccfr.Make (Kuhn)

let first_interval_halves () =
  let plain = K.create ~seed:4 External
  and lin = K.create ~seed:4 ~weighting:Linear ~interval:10 External in
  K.run plain 10;
  K.run lin 10;
  let halved rows =
    List.map (fun (k, a) -> (k, Array.map (fun x -> x /. 2.) a)) rows
  in
  let check name a b =
    List.iter2
      (fun (k, x) (_, y) ->
        Alcotest.(check (array (float 0.))) (name ^ " " ^ k) x y)
      a b
  in
  check "regret" (halved (K.regrets plain)) (K.regrets lin);
  check "strategy sum" (halved (K.strategy_sums plain)) (K.strategy_sums lin)

let rejects_interval () =
  Alcotest.check_raises "interval 0"
    (Invalid_argument "Mccfr.create: interval must be positive") (fun () ->
      ignore (K.create ~interval:0 External))

module L = Mccfr.Make (Leduc)
module BL = Best_response.Make (Leduc)

let linear_helps_early () =
  let mean weighting =
    List.fold_left ( +. ) 0.
      (List.init 8 (fun seed ->
           let t = L.create ~seed ~weighting External in
           L.run t 1_000;
           BL.exploitability (L.average_strategy t)))
    /. 8.
  in
  let plain = mean Vanilla and lin = mean Linear in
  if not (lin < 0.85 *. plain) then
    Alcotest.failf "linear %.4f, uniform %.4f" lin plain

let tests =
  [
    Alcotest.test_case "discounting is weighting" `Quick telescopes;
    Alcotest.test_case "cfr+ floors regrets" `Quick plus_floors;
    Alcotest.test_case "first interval halves" `Quick first_interval_halves;
    Alcotest.test_case "interval range" `Quick rejects_interval;
    Alcotest.test_case "linear helps early on leduc" `Quick linear_helps_early;
  ]
