open Cfr_solver
open Report
open Cmdliner

let board =
  let parse s =
    match String.split_on_char ':' s with
    | [ hw; shots ] -> (
        match
          ( String.split_on_char 'x' hw |> List.map int_of_string_opt,
            int_of_string_opt shots )
        with
        | [ Some h; Some w ], Some n when h > 0 && w > 0 && n > 0 -> Ok (h, w, n)
        | _ -> Error (`Msg ("bad board " ^ s)))
    | _ -> Error (`Msg ("expected HxW:SHOTS, got " ^ s))
  in
  let print ppf (h, w, n) = Format.fprintf ppf "%dx%d:%d" h w n in
  Arg.conv (parse, print)

let sizes (t : Multi_tree.t) =
  let seqs = [| 1; 1 |] and infos = [| 0; 0 |] in
  Array.iteri
    (fun k p ->
      infos.(p) <- infos.(p) + 1;
      seqs.(p) <- seqs.(p) + Array.length t.labels.(k))
    t.owner;
  let rec leaves = function
    | Multi_tree.Leaf _ -> 1
    | Decision (_, ch) | Chance (_, ch) ->
        Array.fold_left (fun n c -> n + leaves c) 0 ch
  in
  (seqs, infos, leaves t.root)

let solve boards ships gamma repeat eps plain lp =
  List.iter
    (fun (height, width, shots) ->
      let c =
        {
          Battleship.height;
          width;
          ships =
            List.map (fun length -> { Battleship.length; value = 1. }) ships;
          shots;
          repeat;
          gamma;
        }
      in
      let t0 = Unix.gettimeofday () in
      let tree = Multi_tree.build (Battleship.game c) in
      let seqs, infos, leaves = sizes tree in
      let plan = Efce_lp.plan tree in
      let g = Optimal.create plan in
      Format.printf "%s, loss multiplier %g@." (Battleship.name c) gamma;
      Format.printf
        "  sequences %d / %d, infosets %d / %d, leaves %d, relevant pairs %d, \
         triggers %d (%.1f s)@."
        seqs.(0) seqs.(1) infos.(0) infos.(1) leaves (Efce_lp.pairs plan)
        (Optimal.triggers g)
        (Unix.gettimeofday () -. t0);
      let uniform = Optimal.uniform g in
      let v = Optimal.values g uniform in
      Format.printf "  uniform play: (%.6f, %.6f), welfare %.6f, max gain %s@."
        v.(0) v.(1)
        (v.(0) +. v.(1))
        (sci (Optimal.gain g uniform));
      let t0 = Unix.gettimeofday () in
      let best, steps = Optimal.bisect ~eps ~predictive:(not plain) g in
      let v = Optimal.values g best.xi in
      let iterations =
        List.fold_left (fun n (st : Optimal.step) -> n + st.iterations) 0 steps
      in
      Format.printf
        "  learned SW-max EFCE: (%.6f, %.6f), welfare %.6f, max gain %s, %d \
         games, %d iterations, %.1f s@."
        v.(0) v.(1)
        (v.(0) +. v.(1))
        (sci best.bounds.gain) (List.length steps) iterations
        (Unix.gettimeofday () -. t0);
      (match
         List.filter_map
           (fun (st : Optimal.step) ->
             if st.feasible = Some false then Some st.tau else None)
           steps
       with
      | [] -> ()
      | taus ->
          Format.printf "  certified: no exact EFCE has welfare above %.6f@."
            (List.fold_left Float.min infinity taus));
      let undecided =
        List.length
          (List.filter (fun (st : Optimal.step) -> st.feasible = None) steps)
      in
      if undecided > 0 then
        Format.printf "  %d bisection steps hit the iteration cap@." undecided;
      if lp then begin
        let t0 = Unix.gettimeofday () in
        match Efce_lp.solve ~exact:true plan with
        | Some r ->
            Format.printf "  exact LP: (%.6f, %.6f), welfare %.6f, %.1f s@."
              r.values.(0) r.values.(1)
              (r.values.(0) +. r.values.(1))
              (Unix.gettimeofday () -. t0)
        | None -> Format.printf "  exact LP: infeasible@."
      end)
    boards

let cmds =
  [
    Cmd.v
      (Cmd.info "battleship"
         ~doc:
           "SW-max EFCE of the general-sum Battleship of Farina et al. (2019) \
            by learning, with the game sizes and the uniform equilibrium.")
      Term.(
        const solve
        $ Arg.(
            value
            & opt (list board) [ (3, 1, 2); (2, 2, 3) ]
            & info [ "boards" ] ~docv:"LIST" ~doc:"Boards as HxW:SHOTS.")
        $ Arg.(
            value
            & opt (list int) [ 1 ]
            & info [ "ships" ] ~docv:"LIST" ~doc:"Ship lengths, all of value 1.")
        $ Arg.(
            value & opt float 2.
            & info [ "gamma" ] ~docv:"G" ~doc:"Loss multiplier.")
        $ Arg.(value & flag & info [ "repeat" ] ~doc:"Allow repeated shots.")
        $ Arg.(
            value & opt float 1e-4
            & info [ "eps" ] ~docv:"E"
                ~doc:"Bisection width and incentive slack.")
        $ Arg.(
            value & flag
            & info [ "plain" ]
                ~doc:"Regret matching+ instead of the predictive variant.")
        $ Arg.(value & flag & info [ "lp" ] ~doc:"Also solve the exact LP."));
  ]
