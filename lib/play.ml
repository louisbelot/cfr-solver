type move = { player : Game.player; infoset : string; action : string }
type result = { payoff : float; moves : move list }

module Make (G : Game.S) = struct
  let hand ~rng ~seat ~bot ~ask =
    let rec go h moves =
      match G.node h with
      | Game.Terminal u ->
          { payoff = Game.sign seat *. u; moves = List.rev moves }
      | Chance outcomes ->
          let outcomes = Array.of_list outcomes in
          go (snd outcomes.(Rng.draw rng (Array.map fst outcomes))) moves
      | Decision { player; infoset; actions } ->
          let action =
            if player = seat then begin
              let action = ask infoset (List.map fst actions) in
              if not (List.mem_assoc action actions) then
                invalid_arg ("Play.hand: no action " ^ action ^ " at " ^ infoset);
              action
            end
            else
              let probs = Strategy.probs bot infoset ~n:(List.length actions) in
              fst (List.nth actions (Rng.draw rng probs))
          in
          go (List.assoc action actions) ({ player; infoset; action } :: moves)
    in
    go G.root []
end
