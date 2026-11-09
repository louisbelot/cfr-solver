open Cfr_solver
open Cmdliner

let algorithms =
  [
    ("cfr", Cfr.Vanilla);
    ("cfr+", Cfr.Plus);
    ("lcfr", Cfr.Linear);
    ("dcfr", Cfr.dcfr);
    ("dcfr+", Cfr.dcfr_plus);
  ]

let mean row =
  let acc = ref 0. in
  Array.iteri (fun b x -> acc := !acc +. (float_of_int b *. x)) row;
  !acc

let support row =
  let picked =
    List.filter (fun b -> row.(b) >= 0.01) (List.init (Array.length row) Fun.id)
  in
  match picked with
  | [ b ] -> string_of_int b
  | _ ->
      String.concat " "
        (List.map (fun b -> Printf.sprintf "%d:%.2f" b row.(b)) picked)

let theory (c : Auction.config) v =
  let x = float_of_int c.top in
  match (c.rule, c.reserve) with
  | All_pay, r when r > 0 -> Float.nan
  | rule, r ->
      x
      *. Auction.Theory.bid rule ~bidders:c.bidders
           ~reserve:(float_of_int r /. x)
           (float_of_int v /. x)

let report (c : Auction.config) s =
  let x = float_of_int c.top in
  Format.printf "%4s" "v";
  for i = 0 to c.bidders - 1 do
    Format.printf "  %-22s" (Printf.sprintf "bidder %d" i)
  done;
  Format.printf "  theory@.";
  for v = 0 to c.top do
    Format.printf "%4d" v;
    Array.iter (fun rows -> Format.printf "  %-22s" (support rows.(v))) s;
    Format.printf "  %.2f@." (theory c v)
  done;
  let gains = Auction.Approx.gains c s in
  Format.printf "@.gains      %s@."
    (String.concat " "
       (Array.to_list (Array.map (Printf.sprintf "%.3e") gains)));
  Format.printf "nash conv  %.3e (%.3e of the top value)@."
    (Auction.Approx.nash_conv c s)
    (Auction.Approx.nash_conv c s /. x);
  Format.printf "revenue    %.4f of the top value, continuous theory %.4f@."
    (Auction.Approx.revenue c s /. x)
    (Auction.Theory.revenue ~bidders:c.bidders
       ~reserve:(float_of_int c.reserve /. x)
       ())

let solve rule bidders top void reserve algorithm iterations symmetric exact =
  let c =
    {
      Auction.bidders;
      top;
      rule;
      ties = (if void then Void else Split);
      reserve;
    }
  in
  let t = Auction.Solver.create ~algorithm ~symmetric c in
  let start = Unix.gettimeofday () in
  Auction.Solver.run t iterations;
  let secs = Unix.gettimeofday () -. start in
  Format.printf "%s, %d iterations of %s%s in %.2fs@.@." (Auction.name c)
    iterations
    (Cfr.algorithm_name algorithm)
    (if symmetric then ", symmetric" else "")
    secs;
  report c (Auction.Solver.average t);
  let cur = Auction.Solver.current t in
  Format.printf "current    nash conv %.3e@." (Auction.Approx.nash_conv c cur);
  if exact then begin
    let q = Auction.rationalize cur in
    let nc = Auction.Exact.nash_conv c q in
    let text = Rat.to_string nc in
    Format.printf "exact      nash conv of the last iterate %s@."
      (if String.length text <= 40 then text
       else Printf.sprintf "%.6e" (Rat.to_float nc));
    Array.iteri
      (fun i rows ->
        Array.iteri
          (fun v row ->
            let mixed =
              List.filter
                (fun b -> not (Rat.is_zero row.(b)))
                (List.init (Array.length row) Fun.id)
            in
            if List.length mixed > 1 && (i = 0 || not symmetric) then
              Format.printf "           bidder %d, v = %d: %s@." i v
                (String.concat ", "
                   (List.map
                      (fun b ->
                        Printf.sprintf "%d w.p. %.6f" b (Rat.to_float row.(b)))
                      mixed)))
          rows)
      q
  end

let rule =
  Arg.(
    value
    & opt
        (enum
           [
             ("first", Auction.First_price);
             ("second", Auction.Second_price);
             ("allpay", Auction.All_pay);
           ])
        Auction.First_price
    & info [ "rule" ] ~docv:"RULE" ~doc:"first, second or allpay.")

let bidders =
  Arg.(value & opt int 2 & info [ "bidders" ] ~docv:"N" ~doc:"Bidders.")

let top =
  Arg.(
    value & opt int 20
    & info [ "top" ] ~docv:"X"
        ~doc:"Values and bids are the integers 0 to $(docv), values uniform.")

let void =
  Arg.(
    value & flag
    & info [ "void" ]
        ~doc:"A tie at the top means nobody wins (default: split evenly).")

let reserve =
  Arg.(
    value & opt int 0
    & info [ "reserve" ] ~docv:"R" ~doc:"Bids below $(docv) lose.")

let algorithm =
  Arg.(
    value
    & opt (enum algorithms) Cfr.Plus
    & info [ "a"; "algorithm" ] ~docv:"ALG"
        ~doc:"cfr, cfr+, lcfr, dcfr or dcfr+.")

let iterations =
  Arg.(
    value & opt int 2_000
    & info [ "n"; "iterations" ] ~docv:"N" ~doc:"Iterations.")

let symmetric =
  Arg.(
    value & flag
    & info [ "symmetric" ]
        ~doc:"All bidders share one strategy (self-play against copies).")

let exact =
  Arg.(
    value & flag
    & info [ "exact" ]
        ~doc:"Check the last iterate in exact rational arithmetic.")

let solve_cmd =
  Cmd.v
    (Cmd.info "auction"
       ~doc:
         "Solve a sealed-bid auction with private values on a grid and compare \
          it with the continuous Bayes-Nash equilibrium.")
    Term.(
      const solve $ rule $ bidders $ top $ void $ reserve $ algorithm
      $ iterations $ symmetric $ exact)

let symmetric_solve c iterations =
  let t = Auction.Solver.create ~algorithm:Cfr.Plus ~symmetric:true c in
  Auction.Solver.run t iterations;
  Auction.Solver.average t

let write file text =
  Option.iter
    (fun f -> Out_channel.with_open_text f (fun oc -> output_string oc text))
    file

let config rule bidders top reserve =
  { Auction.bidders; top; rule; ties = Split; reserve }

let bids top iterations csv svg =
  let x = float_of_int top in
  let runs =
    [
      ("first price, 2", Auction.First_price, 2);
      ("first price, 3", First_price, 3);
      ("first price, 5", First_price, 5);
      ("all-pay, 2", All_pay, 2);
      ("all-pay, 5", All_pay, 5);
    ]
  in
  let rows = Buffer.create 4096 in
  Buffer.add_string rows "auction,bidders,value,mean_bid,theory\n";
  Format.printf "%-16s %10s %14s %10s %10s@." "auction" "nash conv"
    "error, v>=1/4" "revenue" "theory";
  let curves =
    List.concat
      (List.mapi
         (fun color (label, rule, n) ->
           let c = config rule n top 0 in
           let s = symmetric_solve c iterations in
           let pts =
             List.init (top + 1) (fun v ->
                 let b = mean s.(0).(v) /. x and th = theory c v /. x in
                 Printf.bprintf rows "%s,%d,%d,%.6f,%.6f\n"
                   (Auction.rule_name rule) n v b th;
                 (float_of_int v /. x, b, th))
           in
           let err =
             List.fold_left
               (fun m (v, b, th) ->
                 if v < 0.25 then m else Float.max m (Float.abs (b -. th)))
               0. pts
           in
           Format.printf "%-16s %10.2e %14.4f %10.4f %10.4f@." label
             (Auction.Approx.nash_conv c s /. x)
             err
             (Auction.Approx.revenue c s /. x)
             (Auction.Theory.revenue ~bidders:n ());
           [
             {
               Plot.name = label;
               color;
               dash = false;
               xy = List.map (fun (v, b, _) -> (v, b)) pts;
             };
             {
               Plot.name = "";
               color;
               dash = true;
               xy = List.map (fun (v, _, th) -> (v, th)) pts;
             };
           ])
         runs)
  in
  write csv (Buffer.contents rows);
  write svg
    (Plot.lines
       ~title:
         (Printf.sprintf "Symmetric CFR+ bids, values 0..%d, %d iterations" top
            iterations)
       ~xlabel:"value / top value" ~ylabel:"mean bid / top value"
       ~note:"dashed: continuous Bayes-Nash bid" curves)

let reserves top iterations step csv svg =
  let x = float_of_int top in
  let rs = List.init (((top - 1) / step) + 1) (fun k -> k * step) in
  let rows = Buffer.create 4096 in
  Buffer.add_string rows "auction,bidders,reserve,revenue,theory\n";
  let curve color label rule n =
    let xy =
      List.map
        (fun r ->
          let c = config rule n top r in
          let rev =
            Auction.Approx.revenue c (symmetric_solve c iterations) /. x
          in
          let th =
            Auction.Theory.revenue ~bidders:n ~reserve:(float_of_int r /. x) ()
          in
          Printf.bprintf rows "%s,%d,%d,%.6f,%.6f\n" (Auction.rule_name rule) n
            r rev th;
          (float_of_int r /. x, rev))
        rs
    in
    let best_r, best =
      List.fold_left
        (fun (br, bv) (r, v) -> if v > bv then (r, v) else (br, bv))
        (0., neg_infinity) xy
    in
    Format.printf "%-16s best reserve %.3f, revenue %.4f (at r = 0: %.4f)@."
      label best_r best
      (snd (List.hd xy));
    { Plot.name = label; color; dash = false; xy }
  in
  let theory_curve color n =
    let xy =
      List.init 101 (fun k ->
          let r = float_of_int k /. 100. in
          (r, Auction.Theory.revenue ~bidders:n ~reserve:r ()))
    in
    { Plot.name = ""; color; dash = true; xy }
  in
  let curves =
    List.map
      (fun (color, label, rule, n) -> curve color label rule n)
      [
        (0, "first price, 2", Auction.First_price, 2);
        (1, "second price, 2", Second_price, 2);
        (2, "first price, 3", First_price, 3);
        (3, "second price, 3", Second_price, 3);
      ]
    @ [ theory_curve 0 2; theory_curve 2 3 ]
  in
  Format.printf "theory: best reserve 0.5, revenue %.4f (n = 2), %.4f (n = 3)@."
    (Auction.Theory.revenue ~bidders:2 ~reserve:0.5 ())
    (Auction.Theory.revenue ~bidders:3 ~reserve:0.5 ());
  write csv (Buffer.contents rows);
  write svg
    (Plot.lines
       ~title:
         (Printf.sprintf "Seller revenue against the reserve, values 0..%d" top)
       ~xlabel:"reserve / top value" ~ylabel:"expected revenue / top value"
       ~note:"dashed: continuous theory, Myerson peak at 1/2" curves)

let file name doc =
  Arg.(value & opt (some string) None & info [ name ] ~docv:"FILE" ~doc)

let bids_cmd =
  let top =
    Arg.(value & opt int 100 & info [ "top" ] ~docv:"X" ~doc:"Top value.")
  and iterations =
    Arg.(
      value & opt int 5_000
      & info [ "n"; "iterations" ] ~docv:"N" ~doc:"Iterations.")
  in
  Cmd.v
    (Cmd.info "auction-bids"
       ~doc:"Bid functions of symmetric CFR+ against the continuous theory.")
    Term.(
      const bids $ top $ iterations
      $ file "csv" "Write the bids to $(docv)."
      $ file "svg" "Write the chart to $(docv).")

let reserve_cmd =
  let top =
    Arg.(value & opt int 40 & info [ "top" ] ~docv:"X" ~doc:"Top value.")
  and iterations =
    Arg.(
      value & opt int 3_000
      & info [ "n"; "iterations" ] ~docv:"N" ~doc:"Iterations.")
  and step =
    Arg.(
      value & opt int 2 & info [ "step" ] ~docv:"S" ~doc:"Reserve grid step.")
  in
  Cmd.v
    (Cmd.info "auction-reserve"
       ~doc:
         "Seller revenue of symmetric CFR+ equilibria as the reserve price \
          moves.")
    Term.(
      const reserves $ top $ iterations $ step
      $ file "csv" "Write the revenue to $(docv)."
      $ file "svg" "Write the chart to $(docv).")

let exact_profile (c : Auction.config) t (x, y) =
  Array.init c.bidders (fun i ->
      let rows = Lemke.behavior t i (if i = 0 then x else y) in
      Array.init (c.top + 1) (fun v -> List.assoc (Auction.key i v) rows))

let random_prior (c : Auction.config) rng =
  let actions = Array.init (c.top + 1) string_of_int in
  List.fold_left
    (fun acc (i, v) ->
      let w =
        Array.init (c.top + 1) (fun _ -> float_of_int (1 + Rng.int rng 9))
      in
      let total = Array.fold_left ( +. ) 0. w in
      Strategy.add (Auction.key i v) ~actions
        ~probs:(Array.map (fun p -> p /. total) w)
        acc)
    Strategy.empty
    (List.concat_map
       (fun i -> List.init (c.top + 1) (fun v -> (i, v)))
       (List.init c.bidders Fun.id))

let exact_support row =
  let picked =
    List.filter
      (fun b -> not (Rat.is_zero row.(b)))
      (List.init (Array.length row) Fun.id)
  in
  match picked with
  | [ b ] -> string_of_int b
  | _ ->
      String.concat " "
        (List.map
           (fun b -> Printf.sprintf "%d:%s" b (Rat.to_string row.(b)))
           picked)

let lemke rule top void reserve priors seed =
  let c =
    {
      Auction.bidders = 2;
      top;
      rule;
      ties = (if void then Void else Split);
      reserve;
    }
  in
  let t = Lemke.build (module (val Auction.game c)) in
  Format.printf "%s: %d + %d sequences@." (Auction.name c) (Lemke.sequences t 0)
    (Lemke.sequences t 1);
  let rng = Rng.make seed in
  let found = ref [] in
  for k = 0 to priors do
    let prior = if k = 0 then Strategy.empty else random_prior c rng in
    let start = Unix.gettimeofday () in
    let sol = Lemke.solve ~prior t in
    let secs = Unix.gettimeofday () -. start in
    let q = exact_profile c t sol.plans in
    let nc = Auction.Exact.nash_conv c q in
    let fresh = not (List.exists (fun q' -> q' = q) !found) in
    if fresh then found := q :: !found;
    Format.printf "%s prior: %d pivots, %.2f s, nash conv %s, revenue %s%s@."
      (if k = 0 then "uniform" else Printf.sprintf "random %d" k)
      sol.pivots secs (Rat.to_string nc)
      (Rat.to_string (Auction.Exact.revenue c q))
      (if fresh then Printf.sprintf ", equilibrium %d" (List.length !found)
       else "")
  done;
  List.iteri
    (fun n q ->
      Format.printf "@.equilibrium %d@.%4s  %-28s  %s@." (n + 1) "v" "bidder 0"
        "bidder 1";
      for v = 0 to c.top do
        Format.printf "%4d  %-28s  %s@." v
          (exact_support q.(0).(v))
          (exact_support q.(1).(v))
      done)
    (List.rev !found)

let priors =
  Arg.(
    value & opt int 0
    & info [ "priors" ] ~docv:"K"
        ~doc:"Also start from $(docv) random completely mixed priors.")

let seed =
  Arg.(value & opt int 1 & info [ "seed" ] ~docv:"S" ~doc:"Prior seed.")

let lemke_cmd =
  Cmd.v
    (Cmd.info "auction-lemke"
       ~doc:
         "Exact Bayes-Nash equilibria of a two-bidder auction from \
          Lemke's           algorithm on the sequence form.")
    Term.(const lemke $ rule $ top $ void $ reserve $ priors $ seed)

let cmds = [ solve_cmd; bids_cmd; reserve_cmd; lemke_cmd ]
