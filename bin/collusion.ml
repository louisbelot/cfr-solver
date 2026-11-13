open Cfr_solver
open Report
open Cmdliner

let timed f =
  let start = Unix.gettimeofday () in
  let r = f () in
  (r, Unix.gettimeofday () -. start)

let nash tree (module G : Multi.S) iterations =
  let module C = Multi_cfr.Make (G) in
  let c = C.create ~algorithm:Cfr.dcfr () in
  C.run c iterations;
  let s = C.average_strategy c in
  (s, Multi_br.nash_conv tree s)

let only tree p sigma =
  List.fold_left
    (fun acc (key, (e : Strategy.entry)) ->
      let k = Array.find_index (String.equal key) tree.Multi_tree.keys in
      match k with
      | Some k when tree.owner.(k) = p ->
          Strategy.add key ~actions:e.actions ~probs:e.probs acc
      | _ -> acc)
    Strategy.empty (Strategy.bindings sigma)

let without tree o sigma =
  List.fold_left
    (fun acc p -> Strategy.union acc (only tree p sigma))
    Strategy.empty
    (List.filter (( <> ) o) [ 0; 1; 2 ])

let run ranks iterations budget csv svg =
  let game = Kuhn_n.game ~ranks 3 in
  let (module G) = game in
  let tree = Multi_tree.build (module G) in
  let sigma, conv = nash tree (module G) iterations in
  let u = Multi_br.values tree sigma in
  Format.printf "%s: %d infosets, dcfr %d iterations, nash conv %s@.@." G.name
    (Array.length tree.keys) iterations (sci conv);
  Format.printf "%-4s %9s %9s %9s %9s %9s %8s %8s %7s@." "adv" "nash" "nash-br"
    "marg-br" "tmecor" "vs-nash" "columns" "support" "secs";
  let show x = if Float.abs x < 5e-5 then 0. else x in
  let runs =
    List.map
      (fun o ->
        let t = Team.build tree ~adversary:o in
        let s, secs = timed (fun () -> Team.solve ?budget t) in
        let guard team =
          let br, _ = Multi_br.best tree o team in
          -.br
        in
        let exploit, _, _ =
          Team.respond ?budget t (Team.realization t (only tree o sigma))
        in
        Format.printf
          "%-4d %+9.4f %+9.4f %+9.4f %+9.4f %+9.4f %8d %8d %7.2f%s@." (o + 1)
          (show (-.u.(o)))
          (show (guard (without tree o sigma)))
          (show (guard (Team.marginals t s.support)))
          (show (Rat.to_float s.value))
          (show exploit) s.columns (List.length s.support) secs
          (if s.optimal then "" else "  (oracle hit its budget)");
        (o, s))
      [ 0; 1; 2 ]
  in
  Format.printf "@.";
  List.iter
    (fun (o, (s : Team.solution)) ->
      Format.printf "adversary %d: tmecor = %s@." (o + 1)
        (Rat.to_string s.value))
    runs;
  Option.iter
    (fun path ->
      let b = Buffer.create 4096 in
      Buffer.add_string b "adversary,iteration,lower,upper\n";
      List.iter
        (fun (o, (s : Team.solution)) ->
          List.iter
            (fun (i, lo, up) ->
              Printf.bprintf b "%d,%d,%.12g,%.12g\n" (o + 1) i lo up)
            s.trace)
        runs;
      write_file path (Buffer.contents b))
    csv;
  Option.iter
    (fun path ->
      let series =
        List.map
          (fun (o, (s : Team.solution)) ->
            {
              Plot.label = Printf.sprintf "adversary %d" (o + 1);
              dashed = false;
              points =
                List.fold_left
                  (fun (best, acc) (i, lo, up) ->
                    let best = Float.min best up in
                    (best, (i, best -. lo) :: acc))
                  (Float.infinity, []) s.trace
                |> snd
                |> List.filter (fun (_, gap) -> gap > 1e-9)
                |> List.rev;
            })
          runs
      in
      write_file path
        (Plot.semilogy
           ~title:(Printf.sprintf "TMECor column generation, %s" G.name)
           ~xlabel:"columns" ~ylabel:"best upper bound minus lower bound" series))
    svg

let uncorrelated ranks iterations gap strong limit seats csv svg =
  let game = Kuhn_n.game ~ranks 3 in
  let (module G) = game in
  let tree = Multi_tree.build (module G) in
  let sigma, conv = nash tree (module G) iterations in
  Format.printf "%s: dcfr %d iterations, nash conv %s, gap %s@.@." G.name
    iterations (sci conv) (sci gap);
  Format.printf "%-4s %9s %9s %9s %9s %9s %9s %7s %7s %8s %8s@." "adv" "nash-br"
    "tme" "tme-up" "tmecor" "price" "root" "nodes" "warm" "pivots" "secs";
  let runs =
    List.map
      (fun o ->
        let t = Team.build tree ~adversary:o in
        let s, secs = timed (fun () -> Tme.solve ~gap ~strong ~limit t) in
        let cor = Rat.to_float (Team.solve t).value in
        let br, _ = Multi_br.best tree o (without tree o sigma) in
        let lo = Rat.to_float s.lower in
        Format.printf
          "%-4d %+9.5f %+9.5f %+9.5f %+9.5f %9.5f %+9.5f %7d %7d %8d %8.2f@."
          (o + 1) (-.br) lo s.upper cor (cor -. lo) s.root s.nodes s.warm
          s.pivots secs;
        (o, s))
      (List.map pred seats)
  in
  Format.printf "@.";
  List.iter
    (fun (o, (s : Tme.result)) ->
      Format.printf "adversary %d: tme >= %s@." (o + 1) (Rat.to_string s.lower))
    runs;
  Option.iter
    (fun path ->
      let b = Buffer.create 4096 in
      Buffer.add_string b "adversary,node,lower,upper\n";
      List.iter
        (fun (o, (s : Tme.result)) ->
          List.iter
            (fun (i, lo, up) ->
              Printf.bprintf b "%d,%d,%.12g,%.12g\n" (o + 1) i lo up)
            s.trace)
        runs;
      write_file path (Buffer.contents b))
    csv;
  Option.iter
    (fun path ->
      let series =
        List.map
          (fun (o, (s : Tme.result)) ->
            {
              Plot.label = Printf.sprintf "adversary %d" (o + 1);
              dashed = false;
              points =
                List.filter_map
                  (fun (i, lo, up) ->
                    if up -. lo > 1e-9 then Some (i, up -. lo) else None)
                  s.trace;
            })
          runs
      in
      write_file path
        (Plot.loglog
           ~title:(Printf.sprintf "TME branch and bound, %s" G.name)
           ~xlabel:"nodes" ~ylabel:"upper bound minus lower bound" series))
    svg

let cmds =
  let ranks =
    Arg.(
      value & opt positive 4
      & info [ "r"; "ranks" ] ~docv:"R" ~doc:"Cards in the deck (3 to 12).")
  and iterations =
    Arg.(
      value & opt positive 20_000
      & info [ "n"; "iterations" ] ~docv:"N"
          ~doc:"DCFR iterations for the Nash reference.")
  and budget =
    Arg.(
      value
      & opt (some positive) None
      & info [ "budget" ] ~docv:"N"
          ~doc:"Branch-and-bound nodes per pricing call before giving up.")
  and file names doc =
    Arg.(value & opt (some string) None & info names ~docv:"FILE" ~doc)
  in
  let strong =
    Arg.(
      value & opt positive 1
      & info [ "strong" ] ~docv:"K"
          ~doc:
            "Strong branching over the $(docv) sequences with the \
             largest              product error (1 branches on the largest one \
             directly).")
  and limit =
    Arg.(
      value & opt positive 20_000
      & info [ "limit" ] ~docv:"N" ~doc:"Stop after $(docv) nodes per seat.")
  and seats =
    Arg.(
      value
      & opt (list int) [ 1; 2; 3 ]
      & info [ "adversary" ] ~docv:"SEATS"
          ~doc:"Comma-separated adversary seats to solve.")
  in
  let gap =
    Arg.(
      value & opt float 1e-6
      & info [ "gap" ] ~docv:"G"
          ~doc:"Stop when the upper and lower bounds are within $(docv).")
  in
  let check ranks n budget csv svg =
    if ranks < 3 || ranks > 12 then Error (`Msg "ranks must be in [3, 12]")
    else Ok (run ranks n budget csv svg)
  in
  let small ranks n gap strong limit seats csv svg =
    if ranks < 3 || ranks > 7 then Error (`Msg "ranks must be in [3, 7]")
    else if List.exists (fun o -> o < 1 || o > 3) seats then
      Error (`Msg "adversary seats are 1, 2 and 3")
    else Ok (uncorrelated ranks n gap strong limit seats csv svg)
  in
  [
    Cmd.v
      (Cmd.info "team"
         ~doc:
           "Two players of three-player Kuhn poker share a payoff and a \
            coordination device against the third: solve the team-maxmin \
            equilibrium with correlation by column generation and compare it \
            with the Nash family.")
      Term.(
        term_result
          (const check $ ranks $ iterations $ budget
          $ file [ "csv" ] "Write both bounds per column to $(docv)."
          $ file [ "svg" ] "Plot the bound gap against columns to $(docv)."));
    Cmd.v
      (Cmd.info "tme"
         ~doc:
           "The same team game without the coordination device: the \
            members             mix independently, which makes the team's \
            problem a nonconvex             bilinear program. Solve it by \
            spatial branch and bound on an RLT             relaxation and \
            compare it with TMECor and the Nash family.")
      Term.(
        term_result
          (const small $ ranks $ iterations $ gap $ strong $ limit $ seats
          $ file [ "csv" ] "Write both bounds per node to $(docv)."
          $ file [ "svg" ] "Plot the bound gap against nodes to $(docv)."));
  ]
