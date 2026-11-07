type 'h node =
  | Terminal of float array
  | Chance of (float * 'h) list
  | Decision of { player : int; infoset : string; actions : (string * 'h) list }

module type S = sig
  type history

  val name : string
  val players : int
  val root : history
  val node : history -> history node
end

module Of_game (G : Game.S) = struct
  type history = G.history

  let name = G.name
  let players = 2
  let root = G.root

  let node h =
    match G.node h with
    | Game.Terminal u -> Terminal [| u; -.u |]
    | Chance outcomes -> Chance outcomes
    | Decision { player; infoset; actions } ->
        let player = match player with P0 -> 0 | P1 -> 1 in
        Decision { player; infoset; actions }
end

module Eval (G : S) = struct
  let values sigma =
    let total = Array.make G.players 0. in
    let rec go h w =
      if w > 0. then
        match G.node h with
        | Terminal u ->
            Array.iteri (fun i x -> total.(i) <- total.(i) +. (w *. x)) u
        | Chance outcomes -> List.iter (fun (p, h') -> go h' (w *. p)) outcomes
        | Decision { infoset; actions; _ } ->
            let probs = Strategy.probs sigma infoset ~n:(List.length actions) in
            List.iteri (fun a (_, h') -> go h' (w *. probs.(a))) actions
    in
    go G.root 1.;
    total

  let infosets player =
    let seen = Hashtbl.create 64 and found = ref [] in
    let rec go h =
      match G.node h with
      | Terminal _ -> ()
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
