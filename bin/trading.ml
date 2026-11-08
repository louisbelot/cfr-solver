open Cfr_solver
open Cmdliner
module F = Figgie
module B = Figgie_bots

let parse_lineup s =
  let names = String.split_on_char ',' s |> List.map String.trim in
  let bots = List.filter_map B.find names in
  if List.length bots <> List.length names then
    Error
      (`Msg
         (Printf.sprintf "unknown bot in %S; known: %s" s
            (String.concat ", " (List.map (fun (b : F.bot) -> b.name) B.all))))
  else if List.length bots < 4 || List.length bots > 5 then
    Error (`Msg "figgie takes 4 or 5 players")
  else Ok (Array.of_list bots)

let tournament lineup games ticks seed =
  Result.map
    (fun bots ->
      let start = Unix.gettimeofday () in
      let r = B.tournament (Rng.make seed) ~games ~ticks bots in
      Format.printf "%d games of %d ticks, seats shuffled, %.2fs@." games ticks
        (Unix.gettimeofday () -. start);
      Format.printf "%-16s %10s %8s@." "bot" "mean pnl" "stderr";
      Array.iteri
        (fun i (s : B.stats) ->
          Format.printf "%-16s %+10.2f %8.2f@." bots.(i).F.name s.mean s.stderr)
        r)
    (parse_lineup lineup)

let parse_hand s =
  let h = Array.make 4 0 in
  let rec go i =
    if i >= String.length s then Ok h
    else
      match
        Array.find_index
          (fun x -> F.letter x = Char.uppercase_ascii s.[i])
          F.suits
      with
      | None -> Error (`Msg (Printf.sprintf "bad suit %c in %S" s.[i] s))
      | Some k ->
          let j = ref (i + 1) in
          while !j < String.length s && s.[!j] >= '0' && s.[!j] <= '9' do
            incr j
          done;
          if !j = i + 1 then
            Error (`Msg (Printf.sprintf "no count after %c" s.[i]))
          else begin
            h.(k) <- h.(k) + int_of_string (String.sub s (i + 1) (!j - i - 1));
            go !j
          end
  in
  go 0

let hand s players =
  Result.bind (parse_hand s) (fun h ->
      let size = Array.fold_left ( + ) 0 h in
      if players < 4 || players > 5 then
        Error (`Msg "figgie takes 4 or 5 players")
      else if size <> 40 / players then
        Error
          (`Msg
             (Printf.sprintf "a %d-player hand has %d cards" players
                (40 / players)))
      else if Array.exists (fun x -> x > 12) h then
        Error (`Msg "at most 12 of a suit")
      else
        let post = F.posterior h in
        let g = F.goal_probs post in
        let others = Array.make (players - 1) (40 / players) in
        Format.printf "expected payout %.2f (ante %d)@.@."
          (F.expected_payout post h others)
          (F.ante players);
        Format.printf "%-6s %6s %6s %8s %8s@." "suit" "held" "goal" "buy" "sell";
        Array.iteri
          (fun i s ->
            Format.printf "%-6s %6d %6.3f %8.2f %8s@."
              (String.make 1 (F.letter s))
              h.(i) g.(i)
              (F.buy_value post h others s)
              (if h.(i) > 0 then
                 Printf.sprintf "%.2f" (F.sell_value post h others s)
               else "-"))
          F.suits;
        Ok ())

let figgie_cmd =
  let lineup =
    Arg.(
      value
      & opt string "fundamentalist,counter,bottom-feeder,noise"
      & info [ "lineup" ] ~docv:"BOTS"
          ~doc:
            "Comma-separated bots, 4 or 5: passive, noise, \
             fundamentalist,              counter, bottom-feeder. \
             fundamentalist:E and counter:E quote E              dollars from \
             their value (default 2).")
  and games = Arg.(value & opt int 2000 & info [ "games" ] ~docv:"N")
  and ticks =
    Arg.(
      value & opt int 200
      & info [ "ticks" ] ~docv:"T"
          ~doc:"Each player acts once per tick, in a random order.")
  and seed = Arg.(value & opt int 1 & info [ "seed" ] ~docv:"S") in
  Cmd.v
    (Cmd.info "figgie"
       ~doc:"Play Figgie games between bots and report each bot's P&L.")
    Term.(term_result (const tournament $ lineup $ games $ ticks $ seed))

let hand_cmd =
  let h =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"HAND" ~doc:"Counts per suit, e.g. S5C2H2D1.")
  and players = Arg.(value & opt int 4 & info [ "players" ] ~docv:"P") in
  Cmd.v
    (Cmd.info "figgie-hand"
       ~doc:
         "Exact posterior over the goal suit for a dealt hand, and what one \
          more or one less card of each suit is worth.")
    Term.(term_result (const hand $ h $ players))

let edge_sweep edges games ticks seed svg =
  let field = [ B.fundamentalist (); B.bottom_feeder; B.noise ] in
  Format.printf "%5s %18s %18s %18s %18s@." "edge" "counter" "fundamentalist"
    "bottom-feeder" "noise";
  let rows =
    List.init (edges + 1) (fun e ->
        let bots =
          Array.of_list (B.counter ~edge:(float_of_int e) () :: field)
        in
        let r = B.tournament (Rng.make seed) ~games ~ticks bots in
        Format.printf "%5d" e;
        Array.iter
          (fun (s : B.stats) ->
            Format.printf " %+10.2f +- %5.2f" s.mean s.stderr)
          r;
        Format.printf "@.";
        (float_of_int e, Array.map (fun (s : B.stats) -> s.mean) r))
  in
  Option.iter
    (fun f ->
      let curve i color name =
        {
          Plot.name;
          color;
          dash = false;
          xy = List.map (fun (e, m) -> (e, m.(i))) rows;
        }
      in
      Out_channel.with_open_text f (fun oc ->
          output_string oc
            (Plot.lines
               ~title:"Figgie: the counter's quote edge against a fixed field"
               ~xlabel:"counter's edge (dollars from its value)"
               ~ylabel:"mean profit per game (dollars)"
               ~note:
                 (Printf.sprintf "%d games per point, %d ticks, seats shuffled"
                    games ticks)
               [
                 curve 0 0 "counter";
                 curve 1 1 "fundamentalist";
                 curve 2 2 "bottom-feeder";
               ])))
    svg

let edge_cmd =
  let edges =
    Arg.(
      value & opt int 8
      & info [ "edges" ] ~docv:"E" ~doc:"Sweep edges 0 to $(docv).")
  and games = Arg.(value & opt int 2000 & info [ "games" ] ~docv:"N")
  and ticks = Arg.(value & opt int 200 & info [ "ticks" ] ~docv:"T")
  and seed = Arg.(value & opt int 1 & info [ "seed" ] ~docv:"S")
  and svg =
    Arg.(value & opt (some string) None & info [ "svg" ] ~docv:"FILE")
  in
  Cmd.v
    (Cmd.info "figgie-edge"
       ~doc:
         "How far from its value the card-counting bot should quote, against a \
          fundamentalist, a bottom-feeder and a noise trader.")
    Term.(const edge_sweep $ edges $ games $ ticks $ seed $ svg)

let cmds = [ figgie_cmd; hand_cmd; edge_cmd ]
