open Multi_tree

type rollout = Own | Uniform | Prefer of string | Fixed of Strategy.t

let rollout_name = function
  | Own -> "own"
  | Uniform -> "uniform"
  | Prefer a -> "prefer:" ^ a
  | Fixed _ -> "fixed"

let continuation tree i p = function
  | Own -> p
  | (Uniform | Prefer _ | Fixed _) as r ->
      let d = Array.copy p in
      Array.iteri
        (fun k labels ->
          if tree.owner.(k) = i then begin
            let n = Array.length labels and base = tree.offset.(k) in
            let probs =
              match r with
              | Fixed s -> Strategy.probs s tree.keys.(k) ~n
              | Prefer a -> (
                  match Array.find_index (String.equal a) labels with
                  | Some j -> Array.init n (fun b -> if b = j then 1. else 0.)
                  | None -> Array.make n (1. /. float n))
              | Own | Uniform -> Array.make n (1. /. float n)
            in
            Array.blit probs 0 d base n
          end)
        tree.labels;
      d

let choose tree i p d =
  let q = Array.make (slots tree) 0. in
  let rec go node w =
    if w = 0. then 0.
    else
      match node with
      | Leaf u -> u.(i)
      | Chance (pr, children) ->
          let v = ref 0. in
          Array.iteri
            (fun j c -> v := !v +. (pr.(j) *. go c (w *. pr.(j))))
            children;
          !v
      | Decision (k, children) when tree.owner.(k) = i ->
          let base = tree.offset.(k) and v = ref 0. in
          Array.iteri
            (fun a c ->
              let x = go c w in
              q.(base + a) <- q.(base + a) +. (w *. x);
              v := !v +. (d.(base + a) *. x))
            children;
          !v
      | Decision (k, children) ->
          let base = tree.offset.(k) and v = ref 0. in
          Array.iteri
            (fun a c ->
              let s = p.(base + a) in
              if s > 0. then v := !v +. (s *. go c (w *. s)))
            children;
          !v
  in
  ignore (go tree.root 1.);
  let best = Array.copy p in
  Array.iteri
    (fun k labels ->
      if tree.owner.(k) = i then begin
        let base = tree.offset.(k) and n = Array.length labels in
        let j = ref 0 in
        for a = 1 to n - 1 do
          if q.(base + a) > q.(base + !j) then j := a
        done;
        for a = 0 to n - 1 do
          best.(base + a) <- (if a = !j then 1. else 0.)
        done
      end)
    tree.labels;
  best

let respond ?(rollout = Own) tree i p =
  let best = choose tree i p (continuation tree i p rollout) in
  ((Multi_br.payoff tree best).(i), best)

let gains ?rollout tree sigma =
  let p = Multi_br.probs tree sigma in
  let u = Multi_br.payoff tree p in
  Array.init tree.players (fun i -> fst (respond ?rollout tree i p) -. u.(i))

let nash_conv ?rollout tree sigma =
  Array.fold_left ( +. ) 0. (gains ?rollout tree sigma)

module Make (G : Multi.S) = struct
  let tree = Multi_tree.build (module G)

  let value ?rollout i sigma =
    fst (respond ?rollout tree i (Multi_br.probs tree sigma))

  let strategy ?rollout i sigma =
    Multi_br.strategy tree i
      (snd (respond ?rollout tree i (Multi_br.probs tree sigma)))

  let gains ?rollout sigma = gains ?rollout tree sigma
  let nash_conv ?rollout sigma = nash_conv ?rollout tree sigma
end
