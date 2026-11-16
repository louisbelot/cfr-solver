open Cfr_solver
open Report
open Cmdliner

let game name items rounds : (module Multi.S) =
  match name with
  | "kuhn" -> (module (val Kuhn_n.game 2))
  | "kuhn3" -> (module (val Kuhn_n.game 3))
  | "leduc" -> (module (val Leduc_n.game 2))
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

let cmds =
  let name =
    Arg.(
      value
      & opt
          (enum
             (List.map
                (fun n -> (n, n))
                [ "kuhn"; "kuhn3"; "leduc"; "sheriff"; "chicken"; "shapley" ]))
          "kuhn3"
      & info [ "g"; "game" ] ~docv:"GAME"
          ~doc:"kuhn, kuhn3, leduc, sheriff, chicken or shapley.")
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
      (Cmd.info "efce-welfare"
         ~doc:
           "Social-welfare-maximizing EFCE of Sheriff from the \
            correlation-plan LP.")
      Term.(
        const welfare
        $ ints [ "items" ] [ 1; 2; 5; 10 ] "Item counts."
        $ ints [ "rounds" ] [ 1 ] "Round counts."
        $ Arg.(value & flag & info [ "exact" ] ~doc:"Exact rational simplex."));
  ]
