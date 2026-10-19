module Make (G : Game.S) = struct
  module E = Eval.Make (G)

  (* For every infoset of [br], the histories in it as (children, weight),
     where weight is the probability that chance and the opponent play to
     reach the history. Histories that chance or the opponent never reach are
     skipped, since they carry zero weight in every comparison below. *)
  let collect br sigma =
    let reach = Hashtbl.create 64 in
    let rec go h w =
      if w > 0. then
        match G.node h with
        | Game.Terminal _ -> ()
        | Chance outcomes -> List.iter (fun (p, h') -> go h' (w *. p)) outcomes
        | Decision { player; infoset; actions } when player = br ->
            Hashtbl.add reach infoset (Array.of_list (List.map snd actions), w);
            List.iter (fun (_, h') -> go h' w) actions
        | Decision { infoset; actions; _ } ->
            let probs = Strategy.probs sigma infoset ~n:(List.length actions) in
            List.iteri (fun a (_, h') -> go h' (w *. probs.(a))) actions
    in
    go G.root 1.;
    reach

  (* Strict comparison keeps ties on the lowest index, as OpenSpiel does. *)
  let argmax n f =
    let best = ref 0 and best_v = ref (f 0) in
    for a = 1 to n - 1 do
      let v = f a in
      if v > !best_v then begin
        best := a;
        best_v := v
      end
    done;
    !best

  (* An infoset's best action depends on the values below it, which depend on
     best actions at deeper infosets of the same player. Perfect recall makes
     that recursion well founded, and memoizing [best] means each infoset is
     decided once, from all of its histories at the same time. Choosing per
     history instead would let the responder see the opponent's cards. *)
  let solve br sigma =
    let reach = collect br sigma in
    let best = Hashtbl.create 64 in
    let s = Game.sign br in
    let rec value h =
      match G.node h with
      | Game.Terminal u -> s *. u
      | Chance outcomes ->
          List.fold_left (fun acc (p, h') -> acc +. (p *. value h')) 0. outcomes
      | Decision { player; infoset; actions } when player = br ->
          value (snd (List.nth actions (best_action infoset)))
      | Decision { infoset; actions; _ } ->
          let probs = Strategy.probs sigma infoset ~n:(List.length actions) in
          List.fold_left
            (fun (acc, a) (_, h') -> (acc +. (probs.(a) *. value h'), a + 1))
            (0., 0) actions
          |> fst
    and best_action key =
      match Hashtbl.find_opt best key with
      | Some a -> a
      | None ->
          let a =
            match Hashtbl.find_all reach key with
            | [] -> 0
            | (children, _) :: _ as hs ->
                argmax (Array.length children) (fun a ->
                    List.fold_left
                      (fun acc (ch, w) -> acc +. (w *. value ch.(a)))
                      0. hs)
          in
          Hashtbl.add best key a;
          a
    in
    (value, best_action)

  let value br sigma =
    let value, _ = solve br sigma in
    value G.root

  let strategy br sigma =
    let _, best_action = solve br sigma in
    List.fold_left
      (fun s (key, actions) ->
        let a = best_action key in
        let probs = Array.mapi (fun b _ -> if a = b then 1. else 0.) actions in
        Strategy.add key ~actions ~probs s)
      Strategy.empty (E.infosets br)

  let nash_conv sigma = value P0 sigma +. value P1 sigma
  let exploitability sigma = nash_conv sigma /. 2.
end
