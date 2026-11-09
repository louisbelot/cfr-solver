let build (tree : Tree.t) player f =
  Array.fold_left
    (fun s (i : Tree.infoset) ->
      if i.player <> player then s
      else Strategy.add i.key ~actions:i.actions ~probs:(f i.actions) s)
    Strategy.empty tree.infosets

let flat actions =
  let n = Array.length actions in
  Array.make n (1. /. float_of_int n)

let uniform tree player = build tree player flat

let prefer labels tree player =
  build tree player (fun actions ->
      match List.find_opt (fun l -> Array.mem l actions) labels with
      | None -> flat actions
      | Some l -> Array.map (fun a -> if a = l then 1. else 0.) actions)

let part (tree : Tree.t) player sigma =
  Array.fold_left
    (fun s (i : Tree.infoset) ->
      if i.player <> player then s
      else
        let n = Array.length i.actions in
        Strategy.add i.key ~actions:i.actions
          ~probs:(Strategy.probs sigma i.key ~n)
          s)
    Strategy.empty tree.infosets

let blend w a b =
  if not (w >= 0. && w <= 1.) then
    invalid_arg "Bots.blend: weight outside [0, 1]";
  List.fold_left
    (fun s (key, (e : Strategy.entry)) ->
      let n = Array.length e.actions in
      let q = Strategy.probs b key ~n in
      Strategy.add key ~actions:e.actions
        ~probs:
          (Array.mapi (fun i x -> (w *. x) +. ((1. -. w) *. q.(i))) e.probs)
        s)
    Strategy.empty (Strategy.bindings a)
