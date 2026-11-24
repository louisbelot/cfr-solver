type move = { player : Game.player; infoset : string; action : string }
type result = { payoff : float; moves : move list }

type state =
  | Turn of {
      infoset : string;
      actions : string list;
      moves : move list;
      resume : string -> state;
    }
  | Over of result

module Make (G : Game.S) = struct
  let start ~rng ~seat ~bot =
    let rec go h moves =
      match G.node h with
      | Game.Terminal u ->
          Over { payoff = Game.sign seat *. u; moves = List.rev moves }
      | Chance outcomes ->
          let outcomes = Array.of_list outcomes in
          go (snd outcomes.(Rng.draw rng (Array.map fst outcomes))) moves
      | Decision { player; infoset; actions } ->
          let take action =
            go (List.assoc action actions) ({ player; infoset; action } :: moves)
          in
          if player = seat then
            Turn
              {
                infoset;
                actions = List.map fst actions;
                moves = List.rev moves;
                resume =
                  (fun action ->
                    if not (List.mem_assoc action actions) then
                      invalid_arg
                        ("Play.hand: no action " ^ action ^ " at " ^ infoset);
                    take action);
              }
          else
            let probs = Strategy.probs bot infoset ~n:(List.length actions) in
            take (fst (List.nth actions (Rng.draw rng probs)))
    in
    go G.root []

  let hand ~rng ~seat ~bot ~ask =
    let rec finish = function
      | Over r -> r
      | Turn { infoset; actions; resume; _ } ->
          finish (resume (ask infoset actions))
    in
    finish (start ~rng ~seat ~bot)
end
