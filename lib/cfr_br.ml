let respond (tree : Multi_tree.t) p others ~iterations =
  let probs = Multi_br.probs tree others in
  let slots = Multi_tree.slots tree in
  let regret = Array.make slots 0. and avg = Array.make slots 0. in
  let cur = Array.make slots 0. in
  let refresh () =
    Array.iteri
      (fun k _ ->
        if tree.owner.(k) = p then begin
          let lo = tree.offset.(k) and hi = tree.offset.(k + 1) in
          let pos = ref 0. in
          for s = lo to hi - 1 do
            pos := !pos +. regret.(s)
          done;
          for s = lo to hi - 1 do
            cur.(s) <-
              (if !pos > 0. then regret.(s) /. !pos
               else 1. /. float_of_int (hi - lo))
          done
        end)
      tree.keys
  in
  let rec go node q own t =
    if q = 0. then 0.
    else
      match node with
      | Multi_tree.Leaf u -> u.(p)
      | Chance (pr, children) ->
          let v = ref 0. in
          Array.iteri
            (fun j c -> v := !v +. (pr.(j) *. go c (q *. pr.(j)) own t))
            children;
          !v
      | Decision (k, children) when tree.owner.(k) = p ->
          let base = tree.offset.(k) in
          let vs =
            Array.mapi (fun a c -> go c q (own *. cur.(base + a)) t) children
          in
          let v = ref 0. in
          Array.iteri (fun a x -> v := !v +. (cur.(base + a) *. x)) vs;
          Array.iteri
            (fun a x ->
              let s = base + a in
              regret.(s) <- regret.(s) +. (q *. (x -. !v));
              avg.(s) <- avg.(s) +. (t *. own *. cur.(s)))
            vs;
          !v
      | Decision (k, children) ->
          let base = tree.offset.(k) and v = ref 0. in
          Array.iteri
            (fun a c ->
              let s = probs.(base + a) in
              if s > 0. then v := !v +. (s *. go c (q *. s) own t))
            children;
          !v
  in
  for t = 1 to iterations do
    refresh ();
    ignore (go tree.root 1. 1. (float_of_int t));
    Array.iteri (fun s r -> if r < 0. then regret.(s) <- 0.) regret
  done;
  let out = Array.make slots 0. in
  Array.iteri
    (fun k _ ->
      if tree.owner.(k) = p then begin
        let lo = tree.offset.(k) and hi = tree.offset.(k + 1) in
        let total = ref 0. in
        for s = lo to hi - 1 do
          total := !total +. avg.(s)
        done;
        for s = lo to hi - 1 do
          out.(s) <-
            (if !total > 0. then avg.(s) /. !total
             else 1. /. float_of_int (hi - lo))
        done
      end)
    tree.keys;
  Multi_br.strategy tree p out
