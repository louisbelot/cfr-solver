open Cfr_solver

module Expected (G : Game.S) = struct
  module R = Reference_cfr.Make (G)

  let add tbl key n a x =
    let row =
      match Hashtbl.find_opt tbl key with
      | Some row -> row
      | None ->
          let row = Array.make n 0. in
          Hashtbl.add tbl key row;
          row
    in
    row.(a) <- row.(a) +. x

  let increments sigma i =
    let regrets = Hashtbl.create 64 and sums = Hashtbl.create 64 in
    let rec go h p0 p1 pc =
      match G.node h with
      | Game.Terminal _ -> ()
      | Chance outcomes ->
          List.iter (fun (q, h') -> go h' p0 p1 (pc *. q)) outcomes
      | Decision { player; infoset; actions } ->
          let n = List.length actions in
          let probs = Strategy.probs sigma infoset ~n in
          let own, opp = match player with P0 -> (p0, p1) | P1 -> (p1, p0) in
          if player = i then begin
            let vs = List.map (fun (_, h') -> R.value sigma h') actions in
            let v =
              List.fold_left ( +. ) 0.
                (List.mapi (fun a va -> probs.(a) *. va) vs)
            in
            List.iteri
              (fun a va ->
                add regrets infoset n a (Game.sign i *. opp *. pc *. (va -. v)))
              vs
          end
          else
            Array.iteri
              (fun a pa -> add sums infoset n a (pc *. own *. pa))
              probs;
          List.iteri
            (fun a (_, h') ->
              match player with
              | P0 -> go h' (p0 *. probs.(a)) p1 pc
              | P1 -> go h' p0 (p1 *. probs.(a)) pc)
            actions
    in
    go G.root 1. 1. 1.;
    (regrets, sums)
end

module Stats = struct
  type t = {
    mutable n : int;
    mutable peak : float;
    sum : float array;
    sq : float array;
  }

  let create k =
    { n = 0; peak = 0.; sum = Array.make k 0.; sq = Array.make k 0. }

  let add t xs =
    t.n <- t.n + 1;
    Array.iteri
      (fun k x ->
        t.peak <- Float.max t.peak (Float.abs x);
        t.sum.(k) <- t.sum.(k) +. x;
        t.sq.(k) <- t.sq.(k) +. (x *. x))
      xs

  let mean t k = t.sum.(k) /. float_of_int t.n

  let stderr t k ~expected =
    let n = float_of_int t.n and m = mean t k in
    let var = Float.max 0. ((t.sq.(k) /. n) -. (m *. m)) in
    Float.sqrt (Float.max var (t.peak *. Float.abs expected) /. n)
end

let flatten rows = Array.concat (List.map snd rows)
let delta after before = Array.map2 ( -. ) (flatten after) (flatten before)

let check_unbiased (module G : Game.S) sampling ~warmup ~samples () =
  let module M = Mccfr.Make (G) in
  let module X = Expected (G) in
  let t = M.create ~seed:7 sampling in
  M.run t warmup;
  let sigma = M.current_strategy t in
  let rows = M.regrets t in
  let expected tbl =
    Array.concat
      (List.map
         (fun (key, row) ->
           match Hashtbl.find_opt tbl key with
           | Some x -> x
           | None -> Array.make (Array.length row) 0.)
         rows)
  in
  List.iter
    (fun i ->
      let regrets, sums = X.increments sigma i in
      let er = expected regrets and es = expected sums in
      let r = Stats.create (Array.length er)
      and s = Stats.create (Array.length es) in
      for _ = 1 to samples do
        let c = M.split t in
        M.traverse c i;
        Stats.add r (delta (M.regrets c) (M.regrets t));
        Stats.add s (delta (M.strategy_sums c) (M.strategy_sums t))
      done;
      let compare name stats expected =
        Array.iteri
          (fun k e ->
            let m = Stats.mean stats k
            and se = Stats.stderr stats k ~expected:e in
            if not (Float.abs (m -. e) <= (5. *. se) +. 1e-9) then
              Alcotest.failf
                "%s %s slot %d: expected %.6f, sampled %.6f +- %.6f"
                (Game.player_to_string i) name k e m se)
          expected
      in
      compare "regret" r er;
      compare "strategy sum" s es)
    [ Game.P0; P1 ]

module K = Mccfr.Make (Kuhn)
module B = Best_response.Make (Kuhn)
module E = Eval.Make (Kuhn)

let solve ?(seed = 1) sampling n =
  let t = K.create ~seed sampling in
  K.run t n;
  t

let weighted_regret_is_zero sampling () =
  QCheck2.Test.check_exn
  @@ QCheck2.Test.make ~count:200 ~name:"sigma-weighted regret"
       QCheck2.Gen.(pair (int_bound 10_000) (int_bound 50))
       (fun (seed, warmup) ->
         let t = solve ~seed sampling warmup in
         let sigma = K.current_strategy t in
         let c = K.split t in
         K.traverse c (if seed mod 2 = 0 then P0 else P1);
         List.for_all2
           (fun (key, after) (_, before) ->
             let probs = Strategy.probs sigma key ~n:(Array.length after) in
             let total = ref 0. and scale = ref 1. in
             Array.iteri
               (fun a x ->
                 let d = x -. before.(a) in
                 total := !total +. (probs.(a) *. d);
                 scale := Float.max !scale (Float.abs d))
               after;
             Float.abs !total <= 1e-9 *. !scale)
           (K.regrets c) (K.regrets t))

let deterministic () =
  let probs seed =
    Strategy.bindings (K.average_strategy (solve ~seed External 500))
    |> List.map (fun (key, (e : Strategy.entry)) -> (key, e.probs))
  in
  Alcotest.(check bool) "same seed" true (probs 3 = probs 3);
  Alcotest.(check bool) "other seed" false (probs 3 = probs 4)

let path_lengths () =
  let lengths sampling =
    List.init 200 (fun seed ->
        let t = K.create ~seed sampling in
        K.traverse t P0;
        K.touched t)
    |> List.sort_uniq compare
  in
  Alcotest.(check (list int)) "external" [ 6; 8 ] (lengths External);
  Alcotest.(check (list int)) "outcome" [ 4; 5 ] (lengths Mccfr.outcome)

let rejects_epsilon () =
  List.iter
    (fun epsilon ->
      match K.create (Outcome { epsilon }) with
      | _ -> Alcotest.failf "accepted epsilon %g" epsilon
      | exception Invalid_argument _ -> ())
    [ 0.; -0.1; 1.5; Float.nan ]

let converges sampling n bound () =
  let runs =
    List.init 5 (fun seed -> K.average_strategy (solve ~seed sampling n))
  in
  let mean f = List.fold_left (fun acc s -> acc +. f s) 0. runs /. 5. in
  let e = mean B.exploitability in
  if not (e <= bound) then Alcotest.failf "exploitability %g after %d" e n;
  let v = mean E.expected_value in
  if not (Float.abs (v +. (1. /. 18.)) <= 2. *. bound) then
    Alcotest.failf "value %g" v

module L = Mccfr.Make (Leduc)
module BL = Best_response.Make (Leduc)
module EL = Eval.Make (Leduc)

let leduc () =
  let t = L.create External in
  L.run t 100_000;
  let s = L.average_strategy t in
  let e = BL.exploitability s in
  if not (e <= 0.06) then Alcotest.failf "exploitability %g" e;
  let v = EL.expected_value s in
  if not (Float.abs (v +. 0.0856) <= 0.02) then Alcotest.failf "value %g" v

let tests =
  [
    Alcotest.test_case "es unbiased on kuhn" `Quick
      (check_unbiased (module Kuhn) External ~warmup:20 ~samples:20_000);
    Alcotest.test_case "os unbiased on kuhn" `Quick
      (check_unbiased (module Kuhn) Mccfr.outcome ~warmup:20 ~samples:20_000);
    Alcotest.test_case "es unbiased on leduc" `Quick
      (check_unbiased (module Leduc) External ~warmup:50 ~samples:4_000);
    Alcotest.test_case "os unbiased on leduc" `Quick
      (check_unbiased (module Leduc) Mccfr.outcome ~warmup:50 ~samples:20_000);
    Alcotest.test_case "es regret is sigma-neutral" `Quick
      (weighted_regret_is_zero External);
    Alcotest.test_case "os regret is sigma-neutral" `Quick
      (weighted_regret_is_zero Mccfr.outcome);
    Alcotest.test_case "same seed, same run" `Quick deterministic;
    Alcotest.test_case "nodes touched" `Quick path_lengths;
    Alcotest.test_case "epsilon range" `Quick rejects_epsilon;
    Alcotest.test_case "es converges on kuhn" `Quick
      (converges External 20_000 0.01);
    Alcotest.test_case "os converges on kuhn" `Quick
      (converges Mccfr.outcome 100_000 0.02);
    Alcotest.test_case "es on leduc" `Quick leduc;
  ]
