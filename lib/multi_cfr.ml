module Make (G : Multi.S) = struct
  type node =
    | Leaf of float array
    | Chance of float array * node array
    | Decision of int * node array

  type t = {
    root : node;
    size : int;
    keys : string array;
    labels : string array array;
    owner : int array;
    offset : int array;
    regret : float array;
    current : float array;
    sum : float array;
    reach : float array;
    vals : float array;
    acts : float array;
    width : int;
    algorithm : Cfr.algorithm;
    mutable weight : float;
    mutable iterations : int;
    mutable touched : int;
  }

  let build () =
    let index = Hashtbl.create 256 and infos = ref [] in
    let size = ref 0 and height = ref 0 and width = ref 1 in
    let id key player actions =
      match Hashtbl.find_opt index key with
      | Some i -> i
      | None ->
          let i = Hashtbl.length index in
          Hashtbl.add index key i;
          infos := (key, player, Array.of_list (List.map fst actions)) :: !infos;
          i
    in
    let rec go h d =
      incr size;
      height := max !height d;
      let children l =
        Array.of_list (List.map (fun (_, h') -> go h' (d + 1)) l)
      in
      match G.node h with
      | Multi.Terminal u -> Leaf u
      | Chance outcomes ->
          Chance (Array.of_list (List.map fst outcomes), children outcomes)
      | Decision { player; infoset; actions } ->
          width := max !width (List.length actions);
          Decision (id infoset player actions, children actions)
    in
    let root = go G.root 0 in
    (root, !size, !height, !width, Array.of_list (List.rev !infos))

  let create ?(algorithm = Cfr.Vanilla) () =
    let root, size, height, width, infos = build () in
    let labels = Array.map (fun (_, _, a) -> a) infos in
    let offset = Array.make (Array.length infos + 1) 0 in
    Array.iteri
      (fun i a -> offset.(i + 1) <- offset.(i) + Array.length a)
      labels;
    let slots = offset.(Array.length infos) in
    {
      root;
      size;
      keys = Array.map (fun (k, _, _) -> k) infos;
      labels;
      owner = Array.map (fun (_, p, _) -> p) infos;
      offset;
      regret = Array.make slots 0.;
      current = Array.make slots 0.;
      sum = Array.make slots 0.;
      reach = Array.make (G.players + 1) 1.;
      vals = Array.make ((height + 2) * G.players) 0.;
      acts = Array.make ((height + 1) * width) 0.;
      width;
      algorithm;
      weight = 1.;
      iterations = 0;
      touched = 0;
    }

  let iterations t = t.iterations
  let touched t = t.touched
  let infosets t = Array.length t.keys

  let counterfactual t p =
    let w = ref 1. in
    for j = 0 to G.players do
      if j <> p then w := !w *. t.reach.(j)
    done;
    !w

  let rec walk t updating node d =
    let n = G.players in
    let o = d * n in
    match node with
    | Leaf u -> Array.blit u 0 t.vals o n
    | Chance (probs, children) ->
        let pc = t.reach.(n) in
        Array.fill t.vals o n 0.;
        for k = 0 to Array.length children - 1 do
          let p = probs.(k) in
          t.reach.(n) <- pc *. p;
          walk t updating children.(k) (d + 1);
          for j = 0 to n - 1 do
            t.vals.(o + j) <- t.vals.(o + j) +. (p *. t.vals.(o + n + j))
          done
        done;
        t.reach.(n) <- pc
    | Decision (i, children) ->
        let p = t.owner.(i) and base = t.offset.(i) and row = d * t.width in
        let own = t.reach.(p) in
        Array.fill t.vals o n 0.;
        for a = 0 to Array.length children - 1 do
          let s = t.current.(base + a) in
          t.reach.(p) <- own *. s;
          walk t updating children.(a) (d + 1);
          t.acts.(row + a) <- t.vals.(o + n + p);
          for j = 0 to n - 1 do
            t.vals.(o + j) <- t.vals.(o + j) +. (s *. t.vals.(o + n + j))
          done
        done;
        t.reach.(p) <- own;
        if updating p then begin
          let cf = counterfactual t p and v = t.vals.(o + p) in
          for a = 0 to Array.length children - 1 do
            let k = base + a in
            t.regret.(k) <- t.regret.(k) +. (cf *. (t.acts.(row + a) -. v));
            t.sum.(k) <- t.sum.(k) +. (t.weight *. own *. t.current.(k))
          done
        end

  let refresh t =
    for i = 0 to Array.length t.keys - 1 do
      let lo = t.offset.(i) and hi = t.offset.(i + 1) in
      let total = ref 0. in
      for k = lo to hi - 1 do
        total := !total +. Float.max t.regret.(k) 0.
      done;
      for k = lo to hi - 1 do
        t.current.(k) <-
          (if !total > 0. then Float.max t.regret.(k) 0. /. !total
           else 1. /. float_of_int (hi - lo))
      done
    done

  let pass t updating n =
    refresh t;
    walk t updating t.root 0;
    t.touched <- t.touched + t.size;
    let pos, neg, _ = Cfr.discounts t.algorithm n in
    if pos <> 1. || neg <> 1. then
      Array.iteri
        (fun i p ->
          if updating p then
            for k = t.offset.(i) to t.offset.(i + 1) - 1 do
              let r = t.regret.(k) in
              t.regret.(k) <- (r *. if r > 0. then pos else neg)
            done)
        t.owner

  let iterate t =
    let n = t.iterations + 1 in
    t.weight <- Cfr.weight t.algorithm n;
    if Cfr.alternating t.algorithm then
      for p = 0 to G.players - 1 do
        pass t (( = ) p) n
      done
    else pass t (fun _ -> true) n;
    t.iterations <- n

  let run t n =
    for _ = 1 to n do
      iterate t
    done

  let export t f a =
    let s = ref Strategy.empty in
    Array.iteri
      (fun i key ->
        let lo = t.offset.(i) in
        let slice = Array.sub a lo (t.offset.(i + 1) - lo) in
        s := Strategy.add key ~actions:t.labels.(i) ~probs:(f slice) !s)
      t.keys;
    !s

  let average_strategy t = export t Regret_matching.normalize t.sum
  let current_strategy t = export t Regret_matching.strategy t.regret
end
