module Make (G : Game.S) = struct
  let expected_value sigma =
    let rec go h =
      match G.node h with
      | Game.Terminal u -> u
      | Chance outcomes ->
          List.fold_left (fun acc (p, h') -> acc +. (p *. go h')) 0. outcomes
      | Decision { infoset; actions; _ } ->
          let probs = Strategy.probs sigma infoset ~n:(List.length actions) in
          List.fold_left
            (fun (acc, a) (_, h') -> (acc +. (probs.(a) *. go h'), a + 1))
            (0., 0) actions
          |> fst
    in
    go G.root

  let infosets player =
    let seen = Hashtbl.create 64 in
    let found = ref [] in
    let rec go h =
      match G.node h with
      | Game.Terminal _ -> ()
      | Chance outcomes -> List.iter (fun (_, h') -> go h') outcomes
      | Decision { player = p; infoset; actions } ->
          if p = player && not (Hashtbl.mem seen infoset) then begin
            Hashtbl.add seen infoset ();
            found := (infoset, Array.of_list (List.map fst actions)) :: !found
          end;
          List.iter (fun (_, h') -> go h') actions
    in
    go G.root;
    List.rev !found
end
