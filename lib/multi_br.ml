open Multi_tree

let probs tree sigma =
  let p = Array.make (slots tree) 0. in
  Array.iteri
    (fun k key ->
      let n = Array.length tree.labels.(k) in
      Array.blit (Strategy.probs sigma key ~n) 0 p tree.offset.(k) n)
    tree.keys;
  p

let payoff tree p =
  let total = Array.make tree.players 0. in
  let rec go node w =
    if w > 0. then
      match node with
      | Leaf u -> Array.iteri (fun i x -> total.(i) <- total.(i) +. (w *. x)) u
      | Chance (q, children) ->
          Array.iteri (fun j c -> go c (w *. q.(j))) children
      | Decision (k, children) ->
          let base = tree.offset.(k) in
          Array.iteri (fun a c -> go c (w *. p.(base + a))) children
  in
  go tree.root 1.;
  total

let values tree sigma = payoff tree (probs tree sigma)

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

let respond tree p br =
  let n = Array.length tree.keys in
  let hs = Array.make n [] and depth = Array.make n (-1) in
  let rec collect node w seq =
    if w > 0. then
      match node with
      | Leaf _ -> ()
      | Chance (q, children) ->
          Array.iteri (fun j c -> collect c (w *. q.(j)) seq) children
      | Decision (k, children) when tree.owner.(k) = br ->
          hs.(k) <- (children, w) :: hs.(k);
          depth.(k) <- seq;
          Array.iter (fun c -> collect c w (seq + 1)) children
      | Decision (k, children) ->
          let base = tree.offset.(k) in
          Array.iteri (fun a c -> collect c (w *. p.(base + a)) seq) children
  in
  collect tree.root 1. 0;
  let best = Array.make n 0 in
  let rec eval = function
    | Leaf u -> u.(br)
    | Chance (q, children) ->
        let v = ref 0. in
        Array.iteri (fun j c -> v := !v +. (q.(j) *. eval c)) children;
        !v
    | Decision (k, children) when tree.owner.(k) = br ->
        eval children.(best.(k))
    | Decision (k, children) ->
        let base = tree.offset.(k) and v = ref 0. in
        Array.iteri
          (fun a c ->
            let s = p.(base + a) in
            if s > 0. then v := !v +. (s *. eval c))
          children;
        !v
  in
  List.init n Fun.id
  |> List.filter (fun k -> depth.(k) >= 0)
  |> List.stable_sort (fun i j -> compare depth.(j) depth.(i))
  |> List.iter (fun k ->
      best.(k) <-
        argmax
          (Array.length tree.labels.(k))
          (fun a ->
            List.fold_left
              (fun acc (children, w) -> acc +. (w *. eval children.(a)))
              0. hs.(k)));
  (eval tree.root, best)

let best tree br sigma =
  let v, best = respond tree (probs tree sigma) br in
  let s = ref Strategy.empty in
  Array.iteri
    (fun k key ->
      if tree.owner.(k) = br then
        let actions = tree.labels.(k) in
        let probs =
          Array.mapi (fun a _ -> if a = best.(k) then 1. else 0.) actions
        in
        s := Strategy.add key ~actions ~probs !s)
    tree.keys;
  (v, !s)

let gains tree sigma =
  let p = probs tree sigma and u = values tree sigma in
  Array.init tree.players (fun i -> fst (respond tree p i) -. u.(i))

let nash_conv tree sigma = Array.fold_left ( +. ) 0. (gains tree sigma)

module Make (G : Multi.S) = struct
  let tree = Multi_tree.build (module G)
  let value br sigma = fst (respond tree (probs tree sigma) br)
  let strategy br sigma = snd (best tree br sigma)
  let gains sigma = gains tree sigma
  let nash_conv sigma = nash_conv tree sigma
end

let strategy (tree : t) p flat =
  let s = ref Strategy.empty in
  Array.iteri
    (fun k key ->
      if tree.owner.(k) = p then
        let n = Array.length tree.labels.(k) in
        s :=
          Strategy.add key ~actions:tree.labels.(k)
            ~probs:(Array.sub flat tree.offset.(k) n)
            !s)
    tree.keys;
  !s
