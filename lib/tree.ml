type node =
  | Terminal of float
  | Chance of { probs : float array; children : node array }
  | Decision of { infoset : int; children : node array }

type infoset = { key : string; player : Game.player; actions : string array }
type t = { root : node; infosets : infoset array }

let build (module G : Game.S) =
  let ids = Hashtbl.create 64 in
  let found = ref [] in
  let id key player actions =
    match Hashtbl.find_opt ids key with
    | Some i -> i
    | None ->
        let i = Hashtbl.length ids in
        Hashtbl.add ids key i;
        found := { key; player; actions = Array.of_list actions } :: !found;
        i
  in
  let rec go h =
    match G.node h with
    | Game.Terminal u -> Terminal u
    | Chance outcomes ->
        let probs = Array.of_list (List.map fst outcomes) in
        Chance
          {
            probs;
            children = Array.of_list (List.map (fun (_, h') -> go h') outcomes);
          }
    | Decision { player; infoset; actions } ->
        let infoset = id infoset player (List.map fst actions) in
        Decision
          {
            infoset;
            children = Array.of_list (List.map (fun (_, h') -> go h') actions);
          }
  in
  let root = go G.root in
  { root; infosets = Array.of_list (List.rev !found) }
