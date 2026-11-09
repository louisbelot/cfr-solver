open Cfr_solver
open Report

let games : (string * (module Game.S)) list =
  [
    ("kuhn", (module Kuhn));
    ("leduc", (module Leduc));
    ("liars", Liars_dice.game Liars_dice.standard);
    ("liars-nowild", Liars_dice.game { Liars_dice.standard with wild = false });
    ("liars-2d3", Liars_dice.game { dice = 2; faces = 3; wild = true });
  ]

type algorithm =
  | Full of Cfr.algorithm
  | Sampled of Mccfr.sampling
  | Entropic of { eta : float; optimistic : bool }

let algorithms =
  [
    Full Vanilla;
    Full Plus;
    Full Linear;
    Full Cfr.dcfr;
    Sampled External;
    Sampled Mccfr.outcome;
  ]

let modern = [ Full Cfr.dcfr_plus; Full Cfr.pcfr_plus; Full Cfr.pdcfr_plus ]

let short_name = function
  | Full a when a = Cfr.dcfr -> "dcfr"
  | Full a when a = Cfr.dcfr_plus -> "dcfr+"
  | Full a when a = Cfr.pdcfr_plus -> "pdcfr+"
  | Full a -> Cfr.algorithm_name a
  | Sampled External -> "es"
  | Sampled (Outcome _) -> "os"
  | Entropic { eta; optimistic } ->
      Printf.sprintf "%s:%g" (if optimistic then "oftrl" else "ftrl") eta

type solver = {
  run : int -> unit;
  iterations : unit -> int;
  touched : unit -> int;
  average : unit -> Strategy.t;
  current : unit -> Strategy.t;
}

type parallel = {
  domains : int;
  mode : Parallel.mode;
  batch : int;
  weighting : Cfr.algorithm;
}

let serial = { domains = 1; mode = Shared; batch = 1000; weighting = Vanilla }

let solver ?(par = serial) (module G : Game.S) seed = function
  | Full algorithm ->
      let module C = Cfr.Make (G) in
      let t = C.create ~algorithm () in
      {
        run = C.run t;
        iterations = (fun () -> C.iterations t);
        touched = (fun () -> C.touched t);
        average = (fun () -> C.average_strategy t);
        current = (fun () -> C.current_strategy t);
      }
  | Entropic { eta; optimistic } ->
      let module D = Dilated.Make (G) in
      let t = D.create ~optimistic ~eta () in
      {
        run = D.run t;
        iterations = (fun () -> D.iterations t);
        touched = (fun () -> D.touched t);
        average = (fun () -> D.average_strategy t);
        current = (fun () -> D.current_strategy t);
      }
  | Sampled sampling when par.domains = 1 ->
      let module M = Mccfr.Make (G) in
      let t =
        M.create ~seed ~weighting:par.weighting ~interval:par.batch sampling
      in
      {
        run = M.run t;
        iterations = (fun () -> M.iterations t);
        touched = (fun () -> M.touched t);
        average = (fun () -> M.average_strategy t);
        current = (fun () -> M.current_strategy t);
      }
  | Sampled sampling ->
      let module P = Parallel.Make (G) in
      let t =
        P.create ~seed ~weighting:par.weighting ~batch:par.batch ~mode:par.mode
          ~domains:par.domains sampling
      in
      {
        run = P.run t;
        iterations = (fun () -> P.iterations t);
        touched = (fun () -> P.touched t);
        average = (fun () -> P.average_strategy t);
        current = (fun () -> P.current_strategy t);
      }

let describe par = function
  | Full a -> Cfr.algorithm_name a
  | Entropic _ as a -> short_name a
  | Sampled s ->
      String.concat ""
        [
          Mccfr.sampling_name s;
          (match par.weighting with
          | Vanilla -> ""
          | w -> " weighted as " ^ Cfr.algorithm_name w);
          (if par.domains = 1 then ""
           else
             Printf.sprintf " on %d domains (%s tables, batch %d)" par.domains
               (Parallel.mode_name par.mode)
               par.batch);
        ]

let max_table = 24

let solve game algorithm seed par iterations json trace show =
  let (module G : Game.S) = List.assoc game games in
  let module B = Best_response.Make (G) in
  let module E = Eval.Make (G) in
  let t = solver ~par (module G) seed algorithm in
  let wall = ref 0. in
  let run_to k =
    let start = Unix.gettimeofday () in
    t.run (k - t.iterations ());
    wall := !wall +. (Unix.gettimeofday () -. start)
  in
  let rows =
    match trace with
    | None ->
        run_to iterations;
        []
    | Some _ ->
        List.map
          (fun k ->
            run_to k;
            (k, B.exploitability (t.average ())))
          (checkpoints iterations)
  in
  let sigma = t.average () in
  let value = E.expected_value sigma in
  let expl = B.exploitability sigma in
  let players = [ Game.P0; P1 ] in
  let keys p = List.map fst (E.infosets p) in
  Format.printf "%s: %d iterations of %s in %.2f s@." G.name iterations
    (describe par algorithm) !wall;
  Format.printf "nodes touched   %d@." (t.touched ());
  Format.printf "game value      %.6f (player 0)@." value;
  Format.printf "exploitability  %s@." (sci expl);
  if Strategy.cardinal sigma <= max_table then
    List.iter
      (fun p ->
        Format.printf "@.%s@.%a@?" (Game.player_to_string p)
          (Strategy.pp ~keys:(keys p))
          sigma)
      players
  else if show = [] then
    Format.printf "@.%d infosets; --json or --show print the strategy@."
      (Strategy.cardinal sigma);
  if show <> [] then Format.printf "@.";
  List.iter
    (fun key ->
      match Strategy.find sigma key with
      | None -> Format.printf "  %s  (not an infoset)@." key
      | Some { actions; probs } ->
          Format.printf "  %s" key;
          Array.iteri
            (fun a p ->
              if p >= 5e-4 then Format.printf "  %s %.3f" actions.(a) p)
            probs;
          Format.printf "@.")
    show;
  try
    Option.iter
      (fun path ->
        Json.Object
          [
            ("game", String G.name);
            ("algorithm", String (describe par algorithm));
            ("iterations", Int iterations);
            ("nodes_touched", Int (t.touched ()));
            ("game_value", Float value);
            ("exploitability", Float expl);
            ( "players",
              List
                (List.map
                   (fun p -> Strategy.to_json ~keys:(keys p) sigma)
                   players) );
          ]
        |> Json.to_string
        |> fun s -> write_file path (s ^ "\n"))
      json;
    Option.iter
      (fun path ->
        rows
        |> List.map (fun (k, e) -> Printf.sprintf "%d,%s\n" k (short_float e))
        |> String.concat ""
        |> ( ^ ) "iteration,exploitability\n"
        |> write_file path)
      trace;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

let rec read_action labels =
  Format.printf "> @?";
  match In_channel.input_line stdin with
  | None -> raise End_of_file
  | Some line -> (
      let s = String.trim line in
      match int_of_string_opt s with
      | Some i when i >= 1 && i <= List.length labels -> List.nth labels (i - 1)
      | _ when List.mem s labels -> s
      | _ ->
          Format.printf "pick one of %s, or its number@."
            (String.concat " " labels);
          read_action labels)

let play game iterations hands seat seed =
  let (module G : Game.S) = List.assoc game games in
  let module B = Best_response.Make (G) in
  let module E = Eval.Make (G) in
  let module P = Play.Make (G) in
  let t = solver (module G) seed (Full Cfr.dcfr) in
  t.run iterations;
  let bot = t.average () in
  let value = E.expected_value bot in
  Format.printf "%s: %d iterations of dcfr, exploitability %s@." G.name
    iterations
    (sci (B.exploitability bot));
  Format.printf "game value of the bot's strategy: %+.4f per hand to player 0@."
    value;
  let rng = Rng.make seed in
  let total = ref 0. and played = ref 0 in
  let ask infoset labels =
    Format.printf "%s    %s@." infoset (String.concat " " labels);
    read_action labels
  in
  (try
     for i = 1 to hands do
       let seat =
         match seat with
         | Some p -> p
         | None -> if i mod 2 = 1 then Game.P0 else P1
       in
       Format.printf "@.hand %d, you are %s@." i (Game.player_to_string seat);
       let r = P.hand ~rng ~seat ~bot ~ask in
       List.iter
         (fun (m : Play.move) ->
           Format.printf "  %-3s %-5s at %s@."
             (if m.player = seat then "you" else "bot")
             m.action m.infoset)
         r.moves;
       total := !total +. r.payoff;
       incr played;
       Format.printf "%s %g, running total %+g@."
         (if r.payoff >= 0. then "you win" else "you lose")
         (Float.abs r.payoff) !total
     done
   with End_of_file -> ());
  if !played > 0 then
    Format.printf "@.%d hand%s, %+g total, %+.3f per hand@." !played
      (if !played = 1 then "" else "s")
      !total
      (!total /. float !played)

let compare_algorithms game algos current seed iterations csv svg =
  let (module G : Game.S) = List.assoc game games in
  let module B = Best_response.Make (G) in
  let budget = iterations * 2 * Tree.size (Tree.build (module G)).root in
  let runs =
    List.map
      (fun algorithm ->
        let t = solver (module G) seed algorithm in
        let cpu = ref 0. and last = ref 0 in
        let rows =
          List.filter_map
            (fun k ->
              let start = Sys.time () in
              while t.touched () < k do
                t.run 1
              done;
              cpu := !cpu +. (Sys.time () -. start);
              let n = t.touched () in
              if n = !last then None
              else begin
                last := n;
                let s = if current then t.current () else t.average () in
                Some (n, B.exploitability s)
              end)
            (log_checkpoints 10 budget)
        in
        (algorithm, rows, !cpu, t.iterations ()))
      algos
  in
  let at k rows =
    match List.rev (List.filter (fun (n, _) -> n <= k) rows) with
    | [] -> "-"
    | (_, e) :: _ -> sci e
  in
  let rec decades k = if k >= budget then [] else k :: decades (k * 10) in
  let which = if current then "current" else "average" in
  Format.printf "%s: exploitability of the %s strategy by nodes touched@.@."
    G.name which;
  Format.printf "%12s" "nodes";
  List.iter (fun (a, _, _, _) -> Format.printf "%10s" (short_name a)) runs;
  Format.printf "@.";
  List.iter
    (fun k ->
      Format.printf "%12d" k;
      List.iter (fun (_, rows, _, _) -> Format.printf "%10s" (at k rows)) runs;
      Format.printf "@.")
    (decades 1000 @ [ budget ]);
  Format.printf "%12s" "iterations";
  List.iter (fun (_, _, _, n) -> Format.printf "%10d" n) runs;
  Format.printf "@.%12s" "cpu (s)";
  List.iter (fun (_, _, cpu, _) -> Format.printf "%10.2f" cpu) runs;
  Format.printf "@.";
  try
    Option.iter
      (fun path ->
        runs
        |> List.concat_map (fun (a, rows, _, _) ->
            List.map
              (fun (k, e) ->
                Printf.sprintf "%s,%d,%s\n" (short_name a) k (short_float e))
              rows)
        |> String.concat ""
        |> ( ^ ) "algorithm,nodes,exploitability\n"
        |> write_file path)
      csv;
    Option.iter
      (fun path ->
        Plot.loglog
          ~title:
            (Printf.sprintf "%s: exploitability of the %s strategy"
               (String.capitalize_ascii G.name)
               which)
          ~xlabel:"nodes touched" ~ylabel:"exploitability"
          (List.map
             (fun (a, rows, _, _) ->
               {
                 Plot.label = short_name a;
                 dashed = (match a with Sampled _ -> true | _ -> false);
                 points = rows;
               })
             runs)
        |> write_file path)
      svg;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

let scale game seed iterations domains batch csv svg =
  let (module G : Game.S) = List.assoc game games in
  let module P = Parallel.Make (G) in
  let module B = Best_response.Make (G) in
  let top = List.fold_left max 1 domains in
  let configs =
    List.map (fun d -> (Parallel.Shared, d)) domains
    @ if top > 1 then [ (Parallel.Private, top) ] else []
  in
  Pool.run (Pool.default ()) top ignore;
  let label (mode, d) =
    if d = 1 then "serial"
    else Printf.sprintf "%s x%d" (Parallel.mode_name mode) d
  in
  let runs =
    List.map
      (fun (mode, d) ->
        let create () = P.create ~seed ~batch ~mode ~domains:d External in
        let t = create () in
        let start = Unix.gettimeofday () in
        P.run t iterations;
        let wall = Unix.gettimeofday () -. start in
        let e = B.exploitability (P.average_strategy t)
        and touched = P.touched t in
        let t = create () in
        let rows =
          List.map
            (fun k ->
              P.run t (k - P.iterations t);
              (k, B.exploitability (P.average_strategy t)))
            (List.filter
               (fun k -> k >= min iterations 1000)
               (log_checkpoints 5 iterations))
        in
        ((mode, d), rows, (wall, touched, e)))
      configs
  in
  let base = match runs with (_, _, (w, _, _)) :: _ -> w | [] -> 1. in
  Format.printf
    "%s: %d external-sampling iterations, batch %d, %d domains available@.@."
    G.name iterations batch
    (Domain.recommended_domain_count ());
  Format.printf "%-12s %9s %9s %8s %15s@." "config" "wall (s)" "Mnodes/s"
    "speedup" "exploitability";
  List.iter
    (fun (c, _, (wall, touched, e)) ->
      Format.printf "%-12s %9.2f %9.1f %8.2f %15s@." (label c) wall
        (float_of_int touched /. wall /. 1e6)
        (base /. wall) (sci e))
    runs;
  try
    Option.iter
      (fun path ->
        runs
        |> List.concat_map (fun (c, rows, _) ->
            List.map
              (fun (k, e) ->
                Printf.sprintf "%s,%d,%s\n" (label c) k (short_float e))
              rows)
        |> String.concat ""
        |> ( ^ ) "config,iterations,exploitability\n"
        |> write_file path)
      csv;
    Option.iter
      (fun path ->
        Plot.loglog
          ~title:
            (Printf.sprintf "%s: external sampling on OCaml 5 domains"
               (String.capitalize_ascii G.name))
          ~xlabel:"iterations, summed over domains" ~ylabel:"exploitability"
          (List.map
             (fun (((mode, _) as c), rows, _) ->
               {
                 Plot.label = label c;
                 dashed = mode = Parallel.Private;
                 points = rows;
               })
             runs)
        |> write_file path)
      svg;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

let sweep ranks iterations domains seed csv svg =
  let d = { Leduc.ranks; suits = 2 } in
  let module Full = (val Leduc.game d) in
  let module B = Best_response.Make (Full) in
  let ks = List.filter (fun k -> k <= ranks) [ 1; 2; 3; 4; 5; 6; 8; 10; 13 ] in
  let first =
    List.concat_map
      (fun f ->
        List.map
          (fun k ->
            ( Buckets.feature_name f,
              k,
              Buckets.abstraction ~round1:(Buckets.round1 ~seed d f k) d ))
          ks)
      Buckets.[ Ehs; Ehs2; Hist 20 ]
  and splits =
    List.fold_left
      (fun acc m ->
        let r = Buckets.round2 d m in
        if List.exists (fun (p : Buckets.t) -> p.labels = r.labels) acc then acc
        else acc @ [ r ])
      []
      [ 1; 2; 3; 4; 6; 8; 12; 16; 24; 32; 48 ]
  in
  let second =
    List.concat_map
      (fun (series, recall, board) ->
        List.map
          (fun round2 ->
            ( series,
              1 + Array.fold_left max 0 round2.Buckets.labels,
              Buckets.abstraction ~round2 ~recall ~board d ))
          splits)
      [
        ("perfect recall", true, false);
        ("imperfect recall", false, false);
        ("board, imperfect", false, true);
      ]
  in
  let jobs =
    Array.of_list ((("exact", ranks, Leduc.exact d) :: first) @ second)
  in
  let results = Array.make (Array.length jobs) (0, 0.) in
  let next = Atomic.make 0 in
  let solve (_, _, abstraction) =
    let module G = (val Leduc.game ~abstraction d) in
    let module A = Abstraction.Make (Full) (G) in
    let module C = Cfr.Make (G) in
    let t = C.create ~algorithm:Plus () in
    C.run t iterations;
    (A.size, B.exploitability (A.lift (C.average_strategy t)))
  in
  let start = Unix.gettimeofday () in
  Pool.run (Pool.default ()) domains (fun _ ->
      let rec loop () =
        let i = Atomic.fetch_and_add next 1 in
        if i < Array.length jobs then begin
          results.(i) <- solve jobs.(i);
          loop ()
        end
      in
      loop ());
  Format.printf
    "%s: %d iterations of cfr+ per abstraction, %d jobs in %.1f s@.@." Full.name
    iterations (Array.length jobs)
    (Unix.gettimeofday () -. start);
  Format.printf "%-28s %10s %15s@." "abstraction" "infosets" "exploitability";
  Array.iteri
    (fun i (_, _, (a : Leduc.abstraction)) ->
      let size, e = results.(i) in
      Format.printf "%-28s %10d %15s@." a.name size (sci e))
    jobs;
  let rows series =
    List.filter_map Fun.id
      (Array.to_list
         (Array.mapi
            (fun i (s, k, _) ->
              if s = series then Some (k, fst results.(i), snd results.(i))
              else None)
            jobs))
  in
  try
    Option.iter
      (fun path ->
        Array.to_list jobs
        |> List.mapi (fun i (s, k, (a : Leduc.abstraction)) ->
            let size, e = results.(i) in
            Printf.sprintf "%s,%d,%s,%d,%s\n" s k a.name size (short_float e))
        |> String.concat ""
        |> ( ^ ) "series,buckets,abstraction,infosets,exploitability\n"
        |> write_file path)
      csv;
    Option.iter
      (fun path ->
        let series x (name, dashed) =
          {
            Plot.label = name;
            dashed;
            points = List.map (fun (k, size, e) -> (x k size, e)) (rows name);
          }
        in
        write_file path
          (Plot.semilogy
             ~title:
               (Printf.sprintf "%s: round 1 abstracted, round 2 exact" Full.name)
             ~xlabel:"round 1 buckets" ~ylabel:"exploitability in the full game"
             (List.map
                (series (fun k _ -> k))
                [ ("ehs", false); ("ehs2", false); ("emd", false) ]));
        write_file
          (Filename.remove_extension path ^ "-round2.svg")
          (Plot.loglog
             ~title:
               (Printf.sprintf
                  "%s: round 1 exact, round 2 in hand-strength buckets"
                  Full.name)
             ~xlabel:"abstract infosets"
             ~ylabel:"exploitability in the full game"
             (List.map
                (series (fun _ size -> size))
                [
                  ("perfect recall", false);
                  ("imperfect recall", true);
                  ("board, imperfect", true);
                  ("exact", false);
                ])))
      svg;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

let alphas = [ 0.5; 1.; 1.5; 2.; 2.5; 3.; 4.; infinity ]
let gammas = [ 0.; 1.; 2.; 3.; 4.; 5.; 6. ]

let slices =
  [
    ( "dcfr+, beta -inf",
      fun alpha gamma -> Cfr.Discounted { alpha; beta = neg_infinity; gamma } );
    ( "dcfr, beta 0",
      fun alpha gamma -> Cfr.Discounted { alpha; beta = 0.; gamma } );
    ( "dcfr, beta 1",
      fun alpha gamma -> Cfr.Discounted { alpha; beta = 1.; gamma } );
    ("pdcfr+", fun alpha gamma -> Cfr.Predictive { alpha; gamma });
  ]

let landscape game iterations domains csv svg =
  let (module G : Game.S) = List.assoc game games in
  let module B = Best_response.Make (G) in
  let module C = Cfr.Make (G) in
  let named = Cfr.[ Plus; dcfr; dcfr_plus; pcfr_plus; pdcfr_plus ] in
  let grid =
    List.concat_map
      (fun (_, make) ->
        List.concat_map
          (fun alpha -> List.map (fun gamma -> make alpha gamma) gammas)
          alphas)
      slices
  in
  let jobs = Array.of_list (named @ grid) in
  let results = Array.make (Array.length jobs) nan and next = Atomic.make 0 in
  let start = Unix.gettimeofday () in
  Pool.run (Pool.default ()) domains (fun _ ->
      let rec loop () =
        let i = Atomic.fetch_and_add next 1 in
        if i < Array.length jobs then begin
          let t = C.create ~algorithm:jobs.(i) () in
          C.run t iterations;
          results.(i) <- B.exploitability (C.average_strategy t);
          loop ()
        end
      in
      loop ());
  let k = List.length named
  and na = List.length alphas
  and ng = List.length gammas in
  let panel s =
    Array.init na (fun a ->
        Array.init ng (fun g -> results.(k + (s * na * ng) + (a * ng) + g)))
  in
  let label x = if x = infinity then "inf" else Printf.sprintf "%g" x in
  Format.printf "%s, %d iterations each, %d settings in %.1f s@.@." G.name
    iterations (Array.length jobs)
    (Unix.gettimeofday () -. start);
  List.iteri
    (fun i a ->
      Format.printf "%-16s %s@." (Cfr.algorithm_name a) (sci results.(i)))
    named;
  List.iteri
    (fun s (name, _) ->
      Format.printf "@.%s: exploitability, alpha down, gamma across@.%6s" name
        "";
      List.iter (fun g -> Format.printf "%10s" (label g)) gammas;
      Format.printf "@.";
      Array.iteri
        (fun a row ->
          Format.printf "%6s" (label (List.nth alphas a));
          Array.iter (fun e -> Format.printf "%10s" (sci e)) row;
          Format.printf "@.")
        (panel s))
    slices;
  try
    Option.iter
      (fun path ->
        List.mapi
          (fun s (name, _) ->
            List.concat
              (List.mapi
                 (fun a alpha ->
                   List.mapi
                     (fun g gamma ->
                       Printf.sprintf "%s,%s,%s,%s\n" name (label alpha)
                         (label gamma)
                         (short_float (panel s).(a).(g)))
                     gammas)
                 alphas))
          slices
        |> List.concat |> String.concat ""
        |> ( ^ ) "slice,alpha,gamma,exploitability\n"
        |> write_file path)
      csv;
    Option.iter
      (fun path ->
        Plot.heatmap
          ~title:
            (Printf.sprintf "%s: exploitability after %d iterations"
               (String.capitalize_ascii G.name)
               iterations)
          ~xlabel:"gamma" ~ylabel:"alpha" ~xs:(List.map label gammas)
          ~ys:(List.map label alphas)
          (List.mapi (fun s (name, _) -> (name, panel s)) slices)
        |> write_file path)
      svg;
    Ok ()
  with Sys_error msg -> Error (`Msg msg)

open Cmdliner

let game =
  let names = List.map fst games in
  Arg.(
    value
    & opt (enum (List.map (fun n -> (n, n)) names)) "kuhn"
    & info [ "g"; "game" ] ~docv:"GAME"
        ~doc:(Printf.sprintf "Game to solve: %s." (String.concat ", " names)))

let named =
  let known = List.map (fun a -> (short_name a, a)) (algorithms @ modern) in
  let parse s =
    match (List.assoc_opt s known, String.split_on_char ':' s) with
    | Some a, _ -> Ok a
    | None, [ (("oftrl" | "ftrl") as k); e ] -> (
        match float_of_string_opt e with
        | Some eta when eta > 0. ->
            Ok (Entropic { eta; optimistic = k = "oftrl" })
        | _ -> Error (`Msg ("bad step size in " ^ s)))
    | None, _ -> Error (`Msg ("unknown algorithm " ^ s))
  in
  Arg.conv (parse, fun ppf a -> Format.pp_print_string ppf (short_name a))

let algorithm =
  Arg.(
    value & opt named (Full Vanilla)
    & info [ "a"; "algorithm" ] ~docv:"ALG"
        ~doc:
          "Algorithm: cfr, cfr+, lcfr (linear), dcfr (discounted, alpha 1.5, \
           beta 0, gamma 2), dcfr+ (alpha 1.5, gamma 4, negative regrets \
           clipped), pcfr+ (predictive CFR+, quadratic averaging), pdcfr+ \
           (predictive DCFR+, alpha 2.3, gamma 5), es (external-sampling \
           MCCFR), os (outcome-sampling MCCFR, exploration 0.6), or oftrl:ETA \
           and ftrl:ETA (optimistic and plain FTRL with the dilated entropy \
           regularizer and step size ETA).")

let seed =
  Arg.(
    value & opt int 1
    & info [ "seed" ] ~docv:"SEED" ~doc:"Random seed for es and os.")

let iterations =
  Arg.(
    value & opt positive 10_000
    & info [ "n"; "iterations" ] ~docv:"N" ~doc:"Number of CFR iterations.")

let batch =
  Arg.(
    value & opt positive 1000
    & info [ "batch" ] ~docv:"B"
        ~doc:
          "Iterations each domain runs between synchronizations, and between \
           discounts when $(b,--weighting) is set.")

let par =
  let domains =
    Arg.(
      value & opt positive 1
      & info [ "domains" ] ~docv:"D"
          ~doc:"Run es or os on $(docv) OCaml domains.")
  and private_ =
    Arg.(
      value & flag
      & info [ "private" ]
          ~doc:
            "Give each domain its own tables and merge them every batch \
             (deterministic) instead of sharing one set of tables.")
  and weighting =
    Arg.(
      value
      & opt
          (enum
             Cfr.
               [
                 ("none", Vanilla);
                 ("lcfr", Linear);
                 ("dcfr", dcfr);
                 ("cfr+", Plus);
               ])
          Vanilla
      & info [ "w"; "weighting" ] ~docv:"W"
          ~doc:
            "Discount es or os regrets and strategy sums like lcfr, dcfr or \
             cfr+ once per batch.")
  in
  Term.(
    const (fun domains private_ batch weighting ->
        {
          domains;
          mode = (if private_ then Parallel.Private else Shared);
          batch;
          weighting;
        })
    $ domains $ private_ $ batch $ weighting)

let json =
  Arg.(
    value
    & opt (some string) None
    & info [ "json" ] ~docv:"FILE" ~doc:"Write the average strategy to $(docv).")

let trace =
  Arg.(
    value
    & opt (some string) None
    & info [ "trace" ] ~docv:"FILE"
        ~doc:
          "Write exploitability at 1, 2, 5, 10, 20, ... iterations to $(docv) \
           as CSV.")

let show =
  Arg.(
    value & opt_all string []
    & info [ "show" ] ~docv:"KEY"
        ~doc:
          "Print the strategy at infoset $(docv), leaving out actions below \
           0.0005. Repeatable.")

let solve_cmd =
  Cmd.v
    (Cmd.info "solve" ~doc:"Run CFR and print the average strategy.")
    Term.(
      term_result
        (const solve $ game $ algorithm $ seed $ par $ iterations $ json $ trace
       $ show))

let play_cmd =
  let hands =
    Arg.(
      value & opt positive 10
      & info [ "hands" ] ~docv:"H" ~doc:"Stop after $(docv) hands.")
  and seat =
    Arg.(
      value
      & opt
          (enum [ ("0", Some Game.P0); ("1", Some Game.P1); ("both", None) ])
          None
      & info [ "seat" ] ~docv:"SEAT"
          ~doc:"Play as player 0, player 1, or both (alternating).")
  and iterations =
    Arg.(
      value & opt positive 2_000
      & info [ "n"; "iterations" ] ~docv:"N"
          ~doc:"DCFR iterations for the bot's strategy.")
  in
  Cmd.v
    (Cmd.info "play"
       ~doc:
         "Play hands against a DCFR strategy in the terminal. Type an action \
          or its number; end of input stops.")
    Term.(const play $ game $ iterations $ hands $ seat $ seed)

let csv =
  Arg.(
    value
    & opt (some string) None
    & info [ "csv" ] ~docv:"FILE"
        ~doc:"Write every algorithm's exploitability curve to $(docv).")

let svg =
  Arg.(
    value
    & opt (some string) None
    & info [ "svg" ] ~docv:"FILE" ~doc:"Plot the curves to $(docv) (log-log).")

let compare_cmd =
  let algos =
    Arg.(
      value
      & opt (list named) algorithms
      & info [ "algorithms" ] ~docv:"LIST"
          ~doc:"Algorithms to run, by the names $(b,--algorithm) takes.")
  and last =
    Arg.(
      value & flag
      & info [ "last" ]
          ~doc:"Track the current strategy instead of the average.")
  in
  Cmd.v
    (Cmd.info "compare"
       ~doc:
         "Run every algorithm on the same budget of nodes touched (what N \
          iterations of an alternating full-tree variant cost) and track \
          exploitability.")
    Term.(
      term_result
        (const compare_algorithms $ game $ algos $ last $ seed $ iterations
       $ csv $ svg))

let landscape_cmd =
  let domains =
    Arg.(
      value & opt positive 1
      & info [ "domains" ] ~docv:"D" ~doc:"Run settings on $(docv) domains.")
  in
  Cmd.v
    (Cmd.info "landscape"
       ~doc:
         "Run discounted CFR over a grid of (alpha, beta, gamma), and \
          predictive DCFR+ over (alpha, gamma), for the same number of \
          iterations and report the exploitability of each.")
    Term.(
      term_result (const landscape $ game $ iterations $ domains $ csv $ svg))

let scale_cmd =
  let domains =
    Arg.(
      value
      & opt (list positive) [ 1; 2; 4; 8 ]
      & info [ "domains" ] ~docv:"LIST"
          ~doc:
            "Domain counts to run with shared tables; the largest also runs \
             with private tables.")
  in
  Cmd.v
    (Cmd.info "scale"
       ~doc:
         "Run external sampling on several domain counts and track           \
          exploitability against wall time.")
    Term.(
      term_result
        (const scale $ game $ seed $ iterations $ domains $ batch $ csv $ svg))

let sweep_cmd =
  let deck =
    let parse s =
      match int_of_string_opt s with
      | Some n when n >= 2 && n <= 13 -> Ok n
      | _ -> Error (`Msg "expected 2 to 13 ranks")
    in
    Arg.conv (parse, Format.pp_print_int)
  in
  let ranks =
    Arg.(
      value & opt deck 13
      & info [ "ranks" ] ~docv:"R"
          ~doc:"Leduc deck with $(docv) ranks, two suits.")
  and iterations =
    Arg.(
      value & opt positive 1000
      & info [ "n"; "iterations" ] ~docv:"N"
          ~doc:"CFR+ iterations per abstraction.")
  and domains =
    Arg.(
      value
      & opt positive (Domain.recommended_domain_count ())
      & info [ "domains" ] ~docv:"D"
          ~doc:"Solve $(docv) abstractions at a time.")
  and csv =
    Arg.(
      value
      & opt (some string) None
      & info [ "csv" ] ~docv:"FILE"
          ~doc:"Write every abstraction's size and exploitability to $(docv).")
  and svg =
    Arg.(
      value
      & opt (some string) None
      & info [ "svg" ] ~docv:"FILE"
          ~doc:
            "Plot the round 1 sweep to $(docv) and the round 2 sweep next \
             to              it, with -round2 added to the name.")
  and seed =
    Arg.(
      value & opt int 1
      & info [ "seed" ] ~docv:"SEED"
          ~doc:"Random seed for the k-medians restarts.")
  in
  Cmd.v
    (Cmd.info "abstract"
       ~doc:
         "Solve card abstractions of a large Leduc deck with CFR+ and measure \
          each abstract strategy's exploitability in the full game.")
    Term.(
      term_result (const sweep $ ranks $ iterations $ domains $ seed $ csv $ svg))

let () =
  exit
    (Cmd.eval
       (Cmd.group
          (Cmd.info "cfr-solver" ~version:"0.1.0"
             ~doc:
               "Equilibrium strategies by counterfactual regret minimization.")
          ([
             solve_cmd;
             play_cmd;
             compare_cmd;
             landscape_cmd;
             scale_cmd;
             sweep_cmd;
           ]
          @ Holdem.cmds @ Multiplayer.cmds @ Exact.cmds @ Auctions.cmds
          @ Markets.cmds @ Trading.cmds @ Exploit.cmds)))
