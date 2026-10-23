open Cfr_solver

let games : (string * (module Game.S)) list =
  [ ("kuhn", (module Kuhn)); ("leduc", (module Leduc)) ]

type algorithm = Full of Cfr.algorithm | Sampled of Mccfr.sampling

let algorithms =
  [
    Full Vanilla;
    Full Plus;
    Full Linear;
    Full Cfr.dcfr;
    Sampled External;
    Sampled Mccfr.outcome;
  ]

let short_name = function
  | Full (Discounted _) -> "dcfr"
  | Full a -> Cfr.algorithm_name a
  | Sampled External -> "es"
  | Sampled (Outcome _) -> "os"

type solver = {
  run : int -> unit;
  iterations : unit -> int;
  touched : unit -> int;
  average : unit -> Strategy.t;
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
      }

let describe par = function
  | Full a -> Cfr.algorithm_name a
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

let checkpoints n =
  let rec go p acc =
    if p >= n then acc
    else go (p * 10) (List.filter (fun k -> k < n) [ 5 * p; 2 * p; p ] @ acc)
  in
  List.rev (n :: go 1 [])

let log_checkpoints per_decade n =
  let steps =
    Float.to_int
      (Float.ceil (Float.log10 (float_of_int n) *. float_of_int per_decade))
  in
  List.init (steps + 1) (fun k ->
      Float.to_int
        (Float.round (10. ** (float_of_int k /. float_of_int per_decade))))
  |> List.filter (fun k -> k < n)
  |> List.cons n |> List.sort_uniq compare

let trim_exponent s =
  match String.index_opt s 'e' with
  | Some i when String.length s - i = 5 && s.[i + 2] = '0' ->
      String.sub s 0 (i + 2) ^ String.sub s (i + 3) 2
  | _ -> s

let sci x = trim_exponent (Printf.sprintf "%.3e" x)
let short_float x = trim_exponent (Printf.sprintf "%.6g" x)

let write_file path contents =
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc contents)

let solve game algorithm seed par iterations json trace =
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
  else
    Format.printf "@.%d infosets; --json writes the strategy@."
      (Strategy.cardinal sigma);
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

let compare_algorithms game seed iterations csv svg =
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
                Some (n, B.exploitability (t.average ()))
              end)
            (log_checkpoints 10 budget)
        in
        (algorithm, rows, !cpu, t.iterations ()))
      algorithms
  in
  let at k rows =
    match List.rev (List.filter (fun (n, _) -> n <= k) rows) with
    | [] -> "-"
    | (_, e) :: _ -> sci e
  in
  let rec decades k = if k >= budget then [] else k :: decades (k * 10) in
  Format.printf
    "%s: exploitability of the average strategy by nodes touched@.@." G.name;
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
            (Printf.sprintf "%s: exploitability of the average strategy"
               (String.capitalize_ascii G.name))
          ~xlabel:"nodes touched" ~ylabel:"exploitability"
          (List.map
             (fun (a, rows, _, _) ->
               {
                 Plot.label = short_name a;
                 dashed = (match a with Sampled _ -> true | Full _ -> false);
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

open Cmdliner

let game =
  let names = List.map fst games in
  Arg.(
    value
    & opt (enum (List.map (fun n -> (n, n)) names)) "kuhn"
    & info [ "g"; "game" ] ~docv:"GAME"
        ~doc:(Printf.sprintf "Game to solve: %s." (String.concat ", " names)))

let algorithm =
  Arg.(
    value
    & opt
        (enum (List.map (fun a -> (short_name a, a)) algorithms))
        (Full Vanilla)
    & info [ "a"; "algorithm" ] ~docv:"ALG"
        ~doc:
          "Algorithm: cfr, cfr+, lcfr (linear), dcfr (discounted, alpha 1.5, \
           beta 0, gamma 2), es (external-sampling MCCFR) or os \
           (outcome-sampling MCCFR, exploration 0.6).")

let seed =
  Arg.(
    value & opt int 1
    & info [ "seed" ] ~docv:"SEED" ~doc:"Random seed for es and os.")

let positive =
  let parse s =
    match int_of_string_opt s with
    | Some n when n > 0 -> Ok n
    | _ -> Error (`Msg "expected a positive integer")
  in
  Arg.conv (parse, Format.pp_print_int)

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

let solve_cmd =
  Cmd.v
    (Cmd.info "solve" ~doc:"Run CFR and print the average strategy.")
    Term.(
      term_result
        (const solve $ game $ algorithm $ seed $ par $ iterations $ json $ trace))

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
  Cmd.v
    (Cmd.info "compare"
       ~doc:
         "Run every algorithm on the same budget of nodes touched (what N \
          iterations of an alternating full-tree variant cost) and track \
          exploitability.")
    Term.(
      term_result
        (const compare_algorithms $ game $ seed $ iterations $ csv $ svg))

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
          [ solve_cmd; compare_cmd; scale_cmd; sweep_cmd ]))
