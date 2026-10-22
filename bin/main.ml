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

let long_name = function
  | Full a -> Cfr.algorithm_name a
  | Sampled s -> Mccfr.sampling_name s

type solver = {
  run : int -> unit;
  iterations : unit -> int;
  touched : unit -> int;
  average : unit -> Strategy.t;
}

let solver (module G : Game.S) seed = function
  | Full algorithm ->
      let module C = Cfr.Make (G) in
      let t = C.create ~algorithm () in
      {
        run = C.run t;
        iterations = (fun () -> C.iterations t);
        touched = (fun () -> C.touched t);
        average = (fun () -> C.average_strategy t);
      }
  | Sampled sampling ->
      let module M = Mccfr.Make (G) in
      let t = M.create ~seed sampling in
      {
        run = M.run t;
        iterations = (fun () -> M.iterations t);
        touched = (fun () -> M.touched t);
        average = (fun () -> M.average_strategy t);
      }

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

let solve game algorithm seed iterations json trace =
  let (module G : Game.S) = List.assoc game games in
  let module B = Best_response.Make (G) in
  let module E = Eval.Make (G) in
  let t = solver (module G) seed algorithm in
  let cpu = ref 0. in
  let run_to k =
    let start = Sys.time () in
    t.run (k - t.iterations ());
    cpu := !cpu +. (Sys.time () -. start)
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
    (long_name algorithm) !cpu;
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
            ("algorithm", String (long_name algorithm));
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

let iterations =
  let positive =
    let parse s =
      match int_of_string_opt s with
      | Some n when n > 0 -> Ok n
      | _ -> Error (`Msg "expected a positive integer")
    in
    Arg.conv (parse, Format.pp_print_int)
  in
  Arg.(
    value & opt positive 10_000
    & info [ "n"; "iterations" ] ~docv:"N" ~doc:"Number of CFR iterations.")

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
        (const solve $ game $ algorithm $ seed $ iterations $ json $ trace))

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

let () =
  exit
    (Cmd.eval
       (Cmd.group
          (Cmd.info "cfr-solver" ~version:"0.1.0"
             ~doc:
               "Equilibrium strategies by counterfactual regret minimization.")
          [ solve_cmd; compare_cmd ]))
