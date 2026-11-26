open Cfr_solver
open Report
open Cmdliner

let game name items rounds : (module Multi.S) =
  match name with
  | "kuhn" -> (module (val Kuhn_n.game 2))
  | "kuhn3" -> (module (val Kuhn_n.game 3))
  | "leduc" -> (module (val Leduc_n.game 2))
  | "leduc3" -> (module (val Leduc_n.game 3))
  | "chicken" -> Bimatrix.chicken
  | "shapley" -> Bimatrix.shapley
  | _ -> Sheriff.game { Sheriff.baseline with items; rounds }

let runs = [ "trigger"; "trigger+"; "cfr"; "cfr+" ]

let trace (module G : Multi.S) run marks =
  let tree = Multi_tree.build (module G) in
  let cpu = ref 0. in
  let step, mixture =
    match run with
    | "trigger" | "trigger+" ->
        let t = Efce.create ~plus:(run = "trigger+") tree in
        ((fun () -> Efce.iterate t), fun () -> Efce.mixture t)
    | _ ->
        let module C = Multi_cfr.Make (G) in
        let algorithm = if run = "cfr+" then Cfr.Plus else Cfr.Vanilla in
        let c = C.create ~algorithm () in
        let mix = Correlated.create (Treeplex.build tree) in
        ( (fun () ->
            C.iterate c;
            Correlated.add mix (Multi_br.probs tree (C.current_strategy c))),
          fun () -> mix )
  in
  let done_ = ref 0 in
  let rows =
    List.map
      (fun k ->
        let t0 = Sys.time () in
        while !done_ < k do
          step ();
          incr done_
        done;
        cpu := !cpu +. (Sys.time () -. t0);
        let m = mixture () in
        (k, Correlated.gap m, Correlated.values m))
      marks
  in
  (rows, !cpu)

let dynamics name items rounds iterations chosen csv svg =
  let g = game name items rounds in
  let (module G : Multi.S) = g in
  let tree = Multi_tree.build g in
  Format.printf "%s: %d players, %d infosets, %d triggers@.@." G.name G.players
    (Array.length tree.keys) (Multi_tree.slots tree);
  let marks = log_checkpoints 4 iterations in
  let results = List.map (fun r -> (r, trace g r marks)) chosen in
  Format.printf "efce gap of the empirical mixture@.%10s" "iteration";
  List.iter (fun (r, _) -> Format.printf "%11s" r) results;
  Format.printf "@.";
  List.iteri
    (fun i k ->
      Format.printf "%10d" k;
      List.iter
        (fun (_, (rows, _)) ->
          let _, gap, _ = List.nth rows i in
          Format.printf "%11s" (sci gap))
        results;
      Format.printf "@.")
    marks;
  Format.printf "@.%-9s %9s  %s@." "" "ms/it" "values";
  List.iter
    (fun (r, (rows, cpu)) ->
      let _, _, u = List.nth rows (List.length rows - 1) in
      Format.printf "%-9s %9.3f  %s@." r
        (1000. *. cpu /. float_of_int iterations)
        (String.concat " "
           (Array.to_list (Array.map (Printf.sprintf "%+.5f") u))))
    results;
  try
    Option.iter
      (fun path ->
        results
        |> List.concat_map (fun (r, (rows, _)) ->
            List.map
              (fun (k, gap, u) ->
                Printf.sprintf "%s,%d,%s,%s\n" r k (short_float gap)
                  (String.concat "," (Array.to_list (Array.map short_float u))))
              rows)
        |> String.concat ""
        |> ( ^ ) "run,iteration,gap,values\n"
        |> write_file path)
      csv;
    Option.iter
      (fun path ->
        Plot.loglog
          ~title:(Printf.sprintf "%s: EFCE gap of the empirical play" G.name)
          ~xlabel:"iterations" ~ylabel:"efce gap"
          (List.map
             (fun (r, (rows, _)) ->
               {
                 Plot.label = r;
                 dashed = String.length r >= 3 && String.sub r 0 3 = "cfr";
                 points = List.map (fun (k, gap, _) -> (k, gap)) rows;
               })
             results)
        |> write_file path)
      svg;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

let coarse_runs = [ "cfr"; "cfr-s"; "average"; "trigger"; "trigger-s" ]
let concepts = Correlated.[ ("efce", Efce); ("efcce", Efcce); ("nfcce", Nfcce) ]

let coarse_trace tree seed run marks =
  let rng = Rng.make seed in
  let step, mixture =
    match run with
    | "trigger" | "trigger-s" ->
        let rng = if run = "trigger" then None else Some rng in
        let t = Efce.create ~plus:true ?rng tree in
        ((fun () -> Efce.iterate t), fun () -> Efce.mixture t)
    | "average" ->
        let t = Coarse.create tree in
        ( (fun () -> Coarse.iterate t),
          fun () ->
            let m = Correlated.create (Treeplex.build tree) in
            Correlated.add m (Coarse.average t);
            m )
    | _ ->
        let rng = if run = "cfr" then None else Some rng in
        let t = Coarse.create ?rng tree in
        ((fun () -> Coarse.iterate t), fun () -> Coarse.mixture t)
  in
  let done_ = ref 0 in
  List.map
    (fun k ->
      while !done_ < k do
        step ();
        incr done_
      done;
      let m = mixture () in
      (k, List.map (fun (_, c) -> Correlated.gap ~concept:c m) concepts))
    marks

let coarse name items rounds iterations seed chosen concept svg =
  let g = game name items rounds in
  let (module G : Multi.S) = g in
  let tree = Multi_tree.build g in
  Format.printf "%s: %d players, %d infosets@.@." G.name G.players
    (Array.length tree.keys);
  let marks = log_checkpoints 4 iterations in
  let results =
    List.map (fun r -> (r, coarse_trace tree seed r marks)) chosen
  in
  let index = ref 0 in
  List.iteri (fun i (n, _) -> if n = concept then index := i) concepts;
  Format.printf "%s gap@.%10s" concept "iteration";
  List.iter (fun (r, _) -> Format.printf "%11s" r) results;
  Format.printf "@.";
  List.iteri
    (fun i k ->
      Format.printf "%10d" k;
      List.iter
        (fun (_, rows) ->
          Format.printf "%11s" (sci (List.nth (snd (List.nth rows i)) !index)))
        results;
      Format.printf "@.")
    marks;
  Format.printf "@.after %d iterations@.%-10s" iterations "";
  List.iter (fun (c, _) -> Format.printf "%11s" c) concepts;
  Format.printf "@.";
  List.iter
    (fun (r, rows) ->
      Format.printf "%-10s" r;
      List.iter
        (fun v -> Format.printf "%11s" (sci v))
        (snd (List.nth rows (List.length rows - 1)));
      Format.printf "@.")
    results;
  try
    Option.iter
      (fun path ->
        Plot.loglog
          ~title:
            (Printf.sprintf "%s: %s gap" G.name
               (String.uppercase_ascii concept))
          ~xlabel:"iterations" ~ylabel:(concept ^ " gap")
          (List.map
             (fun (r, rows) ->
               {
                 Plot.label = r;
                 dashed = String.ends_with ~suffix:"-s" r;
                 points =
                   List.map (fun (k, gaps) -> (k, List.nth gaps !index)) rows;
               })
             results)
        |> write_file path)
      svg;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

let published =
  [
    ((1, 1), (3.00, 2.00));
    ((2, 1), (8.00, 2.00));
    ((3, 1), (3.89, 1.43));
    ((5, 1), (2.28, 1.26));
    ((10, 1), (1.76, 0.93));
    ((1, 2), (3.00, 2.00));
    ((2, 2), (8.00, 2.00));
    ((5, 2), (8.00, 2.00));
    ((10, 2), (7.26, 1.82));
  ]

let welfare items rounds exact =
  Format.printf "%5s %6s %6s %6s  %-18s %-14s %8s@." "items" "rounds" "pairs"
    "rows" "SW-max EFCE" "Farina et al." "seconds";
  List.iter
    (fun r ->
      List.iter
        (fun n ->
          let tree =
            Multi_tree.build
              (Sheriff.game { Sheriff.baseline with items = n; rounds = r })
          in
          let plan = Efce_lp.plan tree in
          let t0 = Unix.gettimeofday () in
          let paper =
            match List.assoc_opt (n, r) published with
            | Some (a, b) -> Printf.sprintf "(%.2f, %.2f)" a b
            | None -> "-"
          in
          match Efce_lp.solve ~exact plan with
          | None ->
              Format.printf "%5d %6d %6d  solver failed@." n r
                (Efce_lp.pairs plan)
          | Some s ->
              Format.printf "%5d %6d %6d %6d  (%.4f, %.4f)   %-14s %8.2f@." n r
                (Efce_lp.pairs plan) s.rows s.values.(0) s.values.(1) paper
                (Unix.gettimeofday () -. t0))
        items)
    rounds

let steer name items rounds iterations alpha decay pays svg payments_svg =
  let g = game name items rounds in
  let (module G : Multi.S) = g in
  let tree = Multi_tree.build g in
  let plan = Efce_lp.plan tree in
  let t0 = Unix.gettimeofday () in
  match Efce_lp.solve ~exact:true plan with
  | None -> Error (`Msg "correlation-plan LP failed")
  | Some r -> (
      let lp = r.values.(0) +. r.values.(1) in
      let med = Multi_tree.build (Mediator.game tree (Efce_lp.mass plan r)) in
      let direct = Mediator.direct med in
      let gains =
        Multi_br.gains med
          (Strategy.union
             (Multi_br.strategy med 0 direct)
             (Multi_br.strategy med 1 direct))
      in
      Format.printf "%s: SW-max EFCE %.4f (%.4f, %.4f), exact LP %.1f s@."
        G.name lp r.values.(0) r.values.(1)
        (Unix.gettimeofday () -. t0);
      Format.printf
        "mediated game: %d infosets (original %d), direct gains %s %s@.@."
        (Array.length med.keys) (Array.length tree.keys)
        (sci gains.(0))
        (sci gains.(1));
      let schedule t =
        if decay then alpha *. (float_of_int t ** -0.25) else alpha
      in
      let runs =
        ( "cfr+",
          Steer.create tree ~direct:(Array.make (Multi_tree.slots tree) 0.) )
        :: ("mediated", Steer.create med ~direct)
        :: List.map
             (fun p ->
               ( Printf.sprintf "P=%g" p,
                 Steer.create ~alpha:schedule ~pay:p med ~direct ))
             pays
      in
      let marks = log_checkpoints 4 iterations in
      let rows =
        List.map
          (fun k ->
            ( k,
              List.map
                (fun (_, t) ->
                  Steer.run t (k - Steer.iterations t);
                  Steer.average t)
                runs ))
          marks
      in
      Format.printf "average welfare@.%10s" "iteration";
      List.iter (fun (n, _) -> Format.printf "%11s" n) runs;
      Format.printf "@.";
      List.iter
        (fun (k, stats) ->
          Format.printf "%10d" k;
          List.iter
            (fun (s : Steer.stats) -> Format.printf "%11.4f" s.welfare)
            stats;
          Format.printf "@.")
        rows;
      Format.printf "@.after %d iterations@.%-10s%11s%11s%11s%11s@." iterations
        "" "off avg" "off last" "paid avg" "paid last";
      List.iter
        (fun (n, t) ->
          if n <> "cfr+" then
            let a = Steer.average t and l = Steer.last t in
            Format.printf "%-10s%11s%11s%11s%11s@." n (sci a.off) (sci l.off)
              (sci a.payment) (sci l.payment))
        runs;
      let chart path ~ylabel ~title f =
        Option.iter
          (fun path ->
            Plot.loglog ~title ~xlabel:"iterations" ~ylabel
              (List.mapi
                 (fun i (n, _) ->
                   {
                     Plot.label = n;
                     dashed = i < 2;
                     points =
                       List.map
                         (fun (k, stats) -> (k, f (List.nth stats i)))
                         rows;
                   })
                 runs
              |> List.filter (fun (s : Plot.series) ->
                  s.label <> "cfr+"
                  && List.exists (fun (_, y) -> y > 0.) s.points))
            |> write_file path)
          path
      in
      try
        chart svg ~ylabel:"SW-max EFCE minus average welfare"
          ~title:(Printf.sprintf "%s: welfare gap to the LP optimum" G.name)
          (fun s -> Float.abs (lp -. s.welfare));
        chart payments_svg ~ylabel:"average payment per round"
          ~title:(Printf.sprintf "%s: cost of steering" G.name) (fun s ->
            s.payment);
        Ok ()
      with Sys_error msg -> Error (`Msg msg))

let optimal items rounds eps plain lp penalties iterations svg =
  let predictive = not plain in
  let instances =
    List.concat_map (fun r -> List.map (fun n -> (n, r)) items) rounds
  in
  Format.printf "%5s %6s %6s %8s  %-18s %9s %9s %-14s %9s %6s %9s %8s@." "items"
    "rounds" "pairs" "triggers" "eps-EFCE" "welfare" "lp" "Farina et al."
    "max gain" "games" "iters" "seconds";
  let solved =
    List.map
      (fun (n, r) ->
        let plan =
          Efce_lp.plan
            (Multi_tree.build
               (Sheriff.game { Sheriff.baseline with items = n; rounds = r }))
        in
        let g = Optimal.create plan in
        let t0 = Unix.gettimeofday () in
        let best, steps = Optimal.bisect ~eps ~predictive g in
        let seconds = Unix.gettimeofday () -. t0 in
        let v = Optimal.values g best.xi in
        let paper =
          match List.assoc_opt (n, r) published with
          | Some (a, b) -> Printf.sprintf "(%.2f, %.2f)" a b
          | None -> "-"
        in
        let optimum =
          if lp then
            Option.map
              (fun (s : Efce_lp.result) -> s.values.(0) +. s.values.(1))
              (Efce_lp.solve ~exact:true plan)
          else None
        in
        let total =
          List.fold_left
            (fun acc (st : Optimal.step) -> acc + st.iterations)
            0 steps
        in
        let open_ =
          List.length
            (List.filter (fun (st : Optimal.step) -> st.feasible = None) steps)
        in
        Format.printf
          "%5d %6d %6d %8d  (%.4f, %.4f)  %9.5f %9s %-14s %9s %6d %9d %8.2f%s@."
          n r (Optimal.size g) (Optimal.triggers g) v.(0) v.(1)
          (v.(0) +. v.(1))
          (match optimum with Some w -> Printf.sprintf "%.5f" w | None -> "-")
          paper (sci best.bounds.gain) (List.length steps) total seconds
          (if open_ > 0 then Printf.sprintf "  (%d undecided)" open_ else "");
        (n, r, g, Option.value optimum ~default:best.tau))
      instances
  in
  if penalties <> [] then begin
    Format.printf
      "@.direct Lagrangian, %d iterations@.%5s %6s %8s %12s %12s %12s %10s@."
      iterations "items" "rounds" "kappa" "lower" "upper" "welfare" "max gain";
    List.iter
      (fun (n, r, g, _) ->
        List.iter
          (fun k ->
            let s = Optimal.solver ~predictive g (Optimal.Penalty k) in
            Optimal.run s iterations;
            let b = Optimal.bounds s in
            Format.printf "%5d %6d %8g %12.6f %12.6f %12.6f %10s@." n r k
              b.lower b.upper b.welfare (sci b.gain))
          penalties)
      solved
  end;
  match (svg, solved) with
  | None, _ | _, [] -> Ok ()
  | Some path, (n, r, g, tau) :: _ -> (
      let marks = log_checkpoints 4 iterations in
      let series label predictive mode =
        let s = Optimal.solver ~predictive g mode in
        {
          Plot.label;
          dashed = not predictive;
          points =
            List.map
              (fun k ->
                Optimal.run s (k - Optimal.iterations s);
                let b = Optimal.bounds s in
                (k, b.upper -. b.lower))
              marks;
        }
      in
      try
        Plot.loglog
          ~title:
            (Printf.sprintf
               "sheriff %d items, %d round%s: saddle gap at tau = %.4f" n r
               (if r = 1 then "" else "s")
               tau)
          ~xlabel:"iterations" ~ylabel:"upper minus lower certificate"
          (series "threshold, rm+" false (Optimal.Threshold tau)
          :: series "threshold, prm+" true (Optimal.Threshold tau)
          :: List.map
               (fun k ->
                 series
                   (Printf.sprintf "penalty %g, prm+" k)
                   true (Optimal.Penalty k))
               penalties)
        |> write_file path;
        Ok ()
      with Sys_error msg -> Error (`Msg msg))

let cmds =
  let name =
    Arg.(
      value
      & opt
          (enum
             (List.map
                (fun n -> (n, n))
                [
                  "kuhn";
                  "kuhn3";
                  "leduc";
                  "leduc3";
                  "sheriff";
                  "chicken";
                  "shapley";
                ]))
          "kuhn3"
      & info [ "g"; "game" ] ~docv:"GAME"
          ~doc:"kuhn, kuhn3, leduc, leduc3, sheriff, chicken or shapley.")
  in
  let items =
    Arg.(
      value & opt int 2
      & info [ "items" ] ~docv:"N" ~doc:"Sheriff: most illegal items.")
  in
  let rounds =
    Arg.(
      value & opt int 1
      & info [ "rounds" ] ~docv:"R" ~doc:"Sheriff: bargaining rounds.")
  in
  let iterations =
    Arg.(
      value & opt positive 1000
      & info [ "n"; "iterations" ] ~docv:"N" ~doc:"Iterations.")
  in
  let chosen =
    Arg.(
      value
      & opt (list (enum (List.map (fun r -> (r, r)) runs))) runs
      & info [ "r"; "runs" ] ~docv:"LIST"
          ~doc:"Any of trigger, trigger+, cfr, cfr+.")
  in
  let file names doc =
    Arg.(value & opt (some string) None & info names ~docv:"FILE" ~doc)
  in
  let ints names default doc =
    Arg.(value & opt (list int) default & info names ~docv:"LIST" ~doc)
  in
  [
    Cmd.v
      (Cmd.info "efce"
         ~doc:
           "Trigger-regret dynamics for extensive-form correlated equilibrium, \
            against CFR's empirical play.")
      Term.(
        term_result
          (const dynamics $ name $ items $ rounds $ iterations $ chosen
          $ file [ "csv" ] "Write the trace as CSV."
          $ file [ "svg" ] "Write the gap chart as SVG."));
    Cmd.v
      (Cmd.info "cce"
         ~doc:
           "EFCE, EFCCE and NFCCE gaps of CFR, sampled CFR (CFR-S), the \
            product of CFR's average strategies and the trigger dynamics.")
      Term.(
        term_result
          (const coarse $ name $ items $ rounds $ iterations
          $ Arg.(
              value & opt int 1
              & info [ "seed" ] ~docv:"N" ~doc:"Sampling seed.")
          $ Arg.(
              value
              & opt
                  (list (enum (List.map (fun r -> (r, r)) coarse_runs)))
                  coarse_runs
              & info [ "r"; "runs" ] ~docv:"LIST"
                  ~doc:"Any of cfr, cfr-s, average, trigger, trigger-s.")
          $ Arg.(
              value
              & opt (enum (List.map (fun (n, _) -> (n, n)) concepts)) "nfcce"
              & info [ "concept" ] ~docv:"C"
                  ~doc:"Gap to chart: efce, efcce or nfcce.")
          $ file [ "svg" ] "Write the gap chart as SVG."));
    Cmd.v
      (Cmd.info "efce-welfare"
         ~doc:
           "Social-welfare-maximizing EFCE of Sheriff from the \
            correlation-plan LP.")
      Term.(
        const welfare
        $ ints [ "items" ] [ 1; 2; 5; 10 ] "Item counts."
        $ ints [ "rounds" ] [ 1 ] "Round counts."
        $ Arg.(value & flag & info [ "exact" ] ~doc:"Exact rational simplex."));
    Cmd.v
      (Cmd.info "optimal"
         ~doc:
           "SW-max EFCE of Sheriff by learning (Zhang et al. 2023): bisection \
            on the thresholded Lagrangian, each zero-sum game solved by regret \
            matching over the correlation-plan polytope (Farina et al. 2019).")
      Term.(
        term_result
          (const optimal
          $ ints [ "items" ] [ 1; 2; 3; 5; 10 ] "Item counts."
          $ ints [ "rounds" ] [ 1 ] "Round counts."
          $ Arg.(
              value & opt float 1e-4
              & info [ "eps" ] ~docv:"E"
                  ~doc:"Bisection width and incentive slack.")
          $ Arg.(
              value & flag
              & info [ "plain" ]
                  ~doc:"Regret matching+ instead of the predictive variant.")
          $ Arg.(
              value & flag
              & info [ "lp" ]
                  ~doc:
                    "Also solve the exact LP; the chart then uses its optimum \
                     as threshold.")
          $ Arg.(
              value
              & opt (list float) []
              & info [ "penalty" ] ~docv:"LIST"
                  ~doc:"Also run the direct Lagrangian with these multipliers.")
          $ Arg.(
              value & opt positive 20_000
              & info [ "n"; "iterations" ] ~docv:"N"
                  ~doc:"Iterations for the penalty runs and the chart.")
          $ file [ "svg" ]
              "Write the saddle-gap chart for the first instance as SVG."));
    Cmd.v
      (Cmd.info "steer"
         ~doc:
           "Steer CFR+ learners to the SW-max EFCE with trajectory payments \
            (Zhang, Farina et al. 2024), the mediator sampling from the LP's \
            correlation plan.")
      Term.(
        term_result
          (const steer $ name $ items $ rounds $ iterations
          $ Arg.(
              value & opt float 0.1
              & info [ "alpha" ] ~docv:"A" ~doc:"Bonus for all-direct play.")
          $ Arg.(
              value & flag
              & info [ "decay" ] ~doc:"Bonus alpha * t^(-1/4) instead of alpha.")
          $ Arg.(
              value
              & opt (list float) [ 1.; 4. ]
              & info [ "pay" ] ~docv:"LIST"
                  ~doc:"Payments to direct players when someone deviates.")
          $ file [ "svg" ] "Write the welfare-gap chart as SVG."
          $ file [ "payments-svg" ] "Write the payment chart as SVG."));
  ]
