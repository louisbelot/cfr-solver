open Cfr_solver
open Report
open Cmdliner

type tree = {
  pot : float;
  limit : float option;
  stack : float;
  bets : float list;
  raises : float list;
  all_in : bool;
  cap : int option;
}

let config ?bets tree streets =
  match tree.limit with
  | Some bet -> Bet_tree.limit ?cap:tree.cap ~pot:tree.pot ~bet streets
  | None ->
      let sizes l =
        List.map (fun f -> Bet_tree.Pot f) l
        @ if tree.all_in then [ Bet_tree.All_in ] else []
      in
      let bets = Option.value bets ~default:tree.bets in
      let s =
        {
          Bet_tree.bets = sizes bets;
          raises = sizes tree.raises;
          cap = Option.value tree.cap ~default:3;
        }
      in
      {
        Bet_tree.pot = tree.pot;
        stack = tree.stack;
        streets = List.init streets (fun _ -> s);
      }

let sizes_to_string l = String.concat "," (List.map (Printf.sprintf "%g") l)

let describe tree =
  match tree.limit with
  | Some bet ->
      Printf.sprintf "pot %g, limit %g, cap %d" tree.pot bet
        (Option.value tree.cap ~default:4)
  | None ->
      Printf.sprintf "pot %g, stack %g, bets %s, raises %s%s, cap %d" tree.pot
        tree.stack
        (sizes_to_string tree.bets)
        (sizes_to_string tree.raises)
        (if tree.all_in then ", all-in" else "")
        (Option.value tree.cap ~default:3)

let street board =
  match Array.length board with 5 -> "river" | 4 -> "turn" | _ -> "flop"

let pct t e = 100. *. e /. Subgame.pot t
let exploitability t prof = pct t (Subgame.exploitability t prof)

let setup board oop ip =
  let board = Hand.parse board in
  (board, Range.of_string ~board oop, Range.of_string ~board ip)

let header board tree t =
  Format.printf "%s %s, %s@." (street board)
    (String.concat " " (Array.to_list (Array.map Hand.to_string board)))
    (describe tree);
  Format.printf "oop %d hands, ip %d hands, %d decision nodes@.@."
    (Range.size (Subgame.range t 0))
    (Range.size (Subgame.range t 1))
    (Subgame.decisions t)

let summary t prof =
  List.iter
    (fun (l : Subgame.line) ->
      Format.printf "  %-3s %-12s"
        (if l.player = 0 then "oop" else "ip")
        (if l.path = "" then "-" else l.path);
      Array.iteri
        (fun a name -> Format.printf "  %s %.3f" name l.freqs.(a))
        l.actions;
      Format.printf "@.")
    (Subgame.lines t prof)

let run_cmd f =
  try f () with Invalid_argument msg | Sys_error msg -> Error (`Msg msg)

let subgame board oop ip tree algorithms iterations csv svg json =
  run_cmd @@ fun () ->
  let board, r0, r1 = setup board oop ip in
  let cfg = config tree (6 - Array.length board) in
  let runs =
    List.map
      (fun (name, algorithm) ->
        let t = Subgame.create ~algorithm cfg ~board r0 r1 in
        let wall = ref 0. in
        let rows =
          List.map
            (fun k ->
              let start = Unix.gettimeofday () in
              Subgame.run t (k - Subgame.iterations t);
              wall := !wall +. (Unix.gettimeofday () -. start);
              (k, exploitability t (Subgame.average t)))
            (log_checkpoints 4 iterations)
        in
        (name, t, rows, !wall))
      algorithms
  in
  let _, t, _, _ = List.hd (List.rev runs) in
  header board tree t;
  Format.printf "exploitability, %% of the pot@.%10s" "iterations";
  List.iter (fun (name, _, _, _) -> Format.printf "%10s" name) runs;
  Format.printf "@.";
  let _, _, first, _ = List.hd runs in
  List.iteri
    (fun i (k, _) ->
      Format.printf "%10d" k;
      List.iter
        (fun (_, _, rows, _) ->
          Format.printf "%10s" (sci (snd (List.nth rows i))))
        runs;
      Format.printf "@.")
    first;
  Format.printf "%10s" "time (s)";
  List.iter (fun (_, _, _, wall) -> Format.printf "%10.2f" wall) runs;
  let name, t, _, _ = List.hd (List.rev runs) in
  let prof = Subgame.average t in
  Format.printf "@.@.%s strategy, oop value %.4f (pot %g)@." name
    (Subgame.value t prof) (Subgame.pot t);
  summary t prof;
  Option.iter
    (fun path ->
      runs
      |> List.concat_map (fun (name, _, rows, _) ->
          List.map
            (fun (k, e) -> Printf.sprintf "%s,%d,%s\n" name k (short_float e))
            rows)
      |> String.concat ""
      |> ( ^ ) "algorithm,iterations,exploitability_pct\n"
      |> write_file path)
    csv;
  Option.iter
    (fun path ->
      Plot.loglog
        ~title:
          (Printf.sprintf "%s %s: exploitability of the average strategy"
             (String.capitalize_ascii (street board))
             (String.concat " "
                (Array.to_list (Array.map Hand.to_string board))))
        ~xlabel:"iterations" ~ylabel:"% of the pot"
        (List.map
           (fun (label, _, points, _) -> { Plot.label; dashed = false; points })
           runs)
      |> write_file path)
    svg;
  Option.iter
    (fun path ->
      write_file path
        (Json.to_string (Strategy.to_json (Subgame.strategy t prof)) ^ "\n"))
    json;
  Ok ()

let no_limit tree =
  if tree.limit <> None then invalid_arg "this experiment needs a no-limit tree"

let translate board oop ip tree iterations (lo, hi, step) csv svg =
  run_cmd @@ fun () ->
  no_limit tree;
  let board, r0, r1 = setup board oop ip in
  let streets = 6 - Array.length board in
  let abstract =
    Subgame.create ~algorithm:Cfr.dcfr (config tree streets) ~board r0 r1
  in
  Subgame.run abstract iterations;
  let prof = Subgame.average abstract in
  header board tree abstract;
  Format.printf "dcfr, %d iterations: %s%% of the pot in its own tree@.@."
    iterations
    (sci (exploitability abstract prof));
  let xs =
    List.init
      (Float.to_int (Float.round ((hi -. lo) /. step)) + 1)
      (fun i -> Float.round (1e6 *. (lo +. (step *. float_of_int i))) /. 1e6)
  in
  Format.printf
    "exploitability (%% of the pot) when both players may also bet x@.";
  Format.printf "%8s" "x";
  List.iter (fun m -> Format.printf "%17s" (Translation.name m)) Translation.all;
  Format.printf "@.";
  let rows =
    List.map
      (fun x ->
        let bets = List.sort_uniq compare (x :: tree.bets) in
        let full = Subgame.create (config ~bets tree streets) ~board r0 r1 in
        let es =
          List.map
            (fun m ->
              exploitability full
                (Subgame.translate ~low:(Translation.low m) ~abstract prof full))
            Translation.all
        in
        Format.printf "%8.3f" x;
        List.iter (fun e -> Format.printf "%17s" (sci e)) es;
        Format.printf "@.";
        (x, es))
      xs
  in
  let percent x = Float.to_int (Float.round (100. *. x)) in
  Option.iter
    (fun path ->
      rows
      |> List.concat_map (fun (x, es) ->
          List.map2
            (fun m e ->
              Printf.sprintf "%s,%g,%s\n" (Translation.name m) x (short_float e))
            Translation.all es)
      |> String.concat ""
      |> ( ^ ) "mapping,x,exploitability_pct\n"
      |> write_file path)
    csv;
  Option.iter
    (fun path ->
      Plot.semilogy
        ~title:
          (Printf.sprintf "Translating an off-tree bet into {%s} pot"
             (sizes_to_string tree.bets))
        ~xlabel:"off-tree bet, % of the pot"
        ~ylabel:"exploitability, % of the pot"
        (List.mapi
           (fun i m ->
             {
               Plot.label = Translation.name m;
               dashed = false;
               points =
                 List.map (fun (x, es) -> (percent x, List.nth es i)) rows;
             })
           Translation.all)
      |> write_file path)
    svg;
  Ok ()

let default_sets =
  [
    [ 1. ];
    [ 0.5 ];
    [ 0.5; 1. ];
    [ 0.33; 0.75 ];
    [ 0.33; 0.75; 1.5 ];
    [ 0.25; 0.5; 1.; 2. ];
  ]

let betsizes board oop ip tree iterations sets csv =
  run_cmd @@ fun () ->
  no_limit tree;
  let board, r0, r1 = setup board oop ip in
  let streets = 6 - Array.length board in
  let sets = (if sets = [] then default_sets else sets) @ [ tree.bets ] in
  List.iter
    (fun s ->
      if not (List.for_all (fun x -> List.mem x tree.bets) s) then
        invalid_arg
          (Printf.sprintf "sizes %s are not all in the full tree (%s)"
             (sizes_to_string s)
             (sizes_to_string tree.bets)))
    sets;
  let full = Subgame.create (config tree streets) ~board r0 r1 in
  header board tree full;
  Format.printf
    "dcfr, %d iterations per tree; off-tree bets mapped pseudo-harmonically@.@."
    iterations;
  Format.printf "%-28s %9s %10s %12s %12s@." "bet sizes (pot)" "decisions"
    "oop value" "own tree" "full tree";
  let rows =
    List.map
      (fun bets ->
        let t =
          Subgame.create ~algorithm:Cfr.dcfr
            (config ~bets tree streets)
            ~board r0 r1
        in
        Subgame.run t iterations;
        let prof = Subgame.average t in
        let own = exploitability t prof in
        let lifted =
          Subgame.translate
            ~low:(Translation.low Pseudo_harmonic)
            ~abstract:t prof full
        in
        let e = exploitability full lifted and v = Subgame.value t prof in
        Format.printf "%-28s %9d %10.4f %12s %12s@." (sizes_to_string bets)
          (Subgame.decisions t) v (sci own) (sci e);
        (bets, Subgame.decisions t, v, own, e))
      sets
  in
  Option.iter
    (fun path ->
      rows
      |> List.map (fun (bets, d, v, own, e) ->
          Printf.sprintf "\"%s\",%d,%s,%s,%s\n" (sizes_to_string bets) d
            (short_float v) (short_float own) (short_float e))
      |> String.concat ""
      |> ( ^ )
           "sizes,decisions,oop_value,exploitability_own_pct,exploitability_full_pct\n"
      |> write_file path)
    csv;
  Ok ()

let board =
  Arg.(
    required
    & opt (some string) None
    & info [ "board" ] ~docv:"CARDS"
        ~doc:
          "Board cards, e.g. \"Qs Jh 8d 5c 2s\" (river) or four cards (turn).")

let range names default who =
  Arg.(
    value & opt string default
    & info names ~docv:"RANGE"
        ~doc:
          (Printf.sprintf
             "%s range: comma-separated hands like AA, AKs, KQo, TT+, A9s+, \
              AhKh, with optional weights (QJs:0.5), or random."
             who))

let oop = range [ "oop" ] "random" "First player's"
let ip = range [ "ip" ] "random" "Second player's"

let tree ~bets =
  let pot =
    Arg.(
      value & opt float 10. & info [ "pot" ] ~docv:"P" ~doc:"Pot at the start.")
  and limit =
    Arg.(
      value
      & opt (some float) None
      & info [ "limit" ] ~docv:"BET"
          ~doc:"Fixed-limit betting with bets and raises of $(docv).")
  and stack =
    Arg.(
      value & opt float 100.
      & info [ "stack" ] ~docv:"S" ~doc:"Effective stack behind (no-limit).")
  and bets =
    Arg.(
      value
      & opt (list float) bets
      & info [ "bets" ] ~docv:"LIST" ~doc:"Bet sizes as fractions of the pot.")
  and raises =
    Arg.(
      value
      & opt (list float) [ 1. ]
      & info [ "raises" ] ~docv:"LIST"
          ~doc:"Raise sizes as fractions of the pot after calling.")
  and all_in =
    Arg.(value & flag & info [ "all-in" ] ~doc:"Allow all-in bets and raises.")
  and cap =
    Arg.(
      value
      & opt (some positive) None
      & info [ "cap" ] ~docv:"N"
          ~doc:"Bets and raises per street (default 4 in limit, 3 otherwise).")
  in
  Term.(
    const (fun pot limit stack bets raises all_in cap ->
        { pot; limit; stack; bets; raises; all_in; cap })
    $ pot $ limit $ stack $ bets $ raises $ all_in $ cap)

let iterations default =
  Arg.(
    value & opt positive default
    & info [ "n"; "iterations" ] ~docv:"N" ~doc:"Iterations per solve.")

let file names doc =
  Arg.(value & opt (some string) None & info names ~docv:"FILE" ~doc)

let subgame_cmd =
  let algorithms =
    let all =
      [
        ("cfr", Cfr.Vanilla);
        ("cfr+", Plus);
        ("lcfr", Linear);
        ("dcfr", Cfr.dcfr);
      ]
    in
    Arg.(
      value
      & opt
          (list (enum (List.map (fun (n, a) -> (n, (n, a))) all)))
          [ ("dcfr", Cfr.dcfr) ]
      & info [ "a"; "algorithms" ] ~docv:"LIST"
          ~doc:"Algorithms to run: cfr, cfr+, lcfr, dcfr.")
  in
  Cmd.v
    (Cmd.info "subgame"
       ~doc:
         "Solve a hold'em river or turn subgame with vectorized CFR over both \
          ranges and track exploitability.")
    Term.(
      term_result
        (const subgame $ board $ oop $ ip
        $ tree ~bets:[ 0.5; 1. ]
        $ algorithms $ iterations 1000
        $ file [ "csv" ] "Write exploitability curves to $(docv)."
        $ file [ "svg" ] "Plot exploitability curves to $(docv)."
        $ file [ "json" ] "Write the last algorithm's strategy to $(docv)."))

let translate_cmd =
  let grid =
    Arg.(
      value
      & opt (t3 ~sep:',' float float float) (0.25, 2., 0.05)
      & info [ "grid" ] ~docv:"LO,HI,STEP" ~doc:"Off-tree bet sizes to try.")
  in
  Cmd.v
    (Cmd.info "translate"
       ~doc:
         "Solve a no-limit subgame with a few bet sizes, then measure how \
          exploitable it is when either player may also bet x, for three \
          action translation mappings.")
    Term.(
      term_result
        (const translate $ board $ oop $ ip
        $ tree ~bets:[ 0.5; 1. ]
        $ iterations 1000 $ grid
        $ file [ "csv" ] "Write the sweep to $(docv)."
        $ file [ "svg" ] "Plot the sweep to $(docv)."))

let betsizes_cmd =
  let sets =
    Arg.(
      value
      & opt_all (list float) []
      & info [ "set" ] ~docv:"LIST"
          ~doc:"A bet-size abstraction to evaluate (repeatable).")
  in
  Cmd.v
    (Cmd.info "betsizes"
       ~doc:
         "Solve several bet-size abstractions of a no-limit subgame and \
          measure each one's exploitability in the tree with every size.")
    Term.(
      term_result
        (const betsizes $ board $ oop $ ip
        $ tree ~bets:[ 0.25; 0.33; 0.5; 0.75; 1.; 1.5; 2. ]
        $ iterations 1000 $ sets
        $ file [ "csv" ] "Write the table to $(docv)."))

let cmds = [ subgame_cmd; translate_cmd; betsizes_cmd ]
