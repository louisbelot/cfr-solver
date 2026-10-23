let check_weights n weights =
  let w = match weights with Some w -> w | None -> Array.make n 1. in
  if Array.length w <> n then invalid_arg "Cluster: weights and points differ";
  if Array.exists (fun x -> not (x > 0.)) w then
    invalid_arg "Cluster: weights must be positive";
  w

let sse ?weights xs labels =
  let w = check_weights (Array.length xs) weights in
  let k = Array.fold_left max (-1) labels + 1 in
  let s0 = Array.make k 0. and s1 = Array.make k 0. and s2 = Array.make k 0. in
  Array.iteri
    (fun i l ->
      s0.(l) <- s0.(l) +. w.(i);
      s1.(l) <- s1.(l) +. (w.(i) *. xs.(i));
      s2.(l) <- s2.(l) +. (w.(i) *. xs.(i) *. xs.(i)))
    labels;
  let total = ref 0. in
  for l = 0 to k - 1 do
    if s0.(l) > 0. then
      total := !total +. Float.max 0. (s2.(l) -. (s1.(l) *. s1.(l) /. s0.(l)))
  done;
  !total

let optimal_1d ?weights xs k =
  if k < 1 then invalid_arg "Cluster.optimal_1d: k must be positive";
  let n = Array.length xs in
  let w = check_weights n weights in
  let order = Array.init n Fun.id in
  Array.stable_sort (fun i j -> compare xs.(i) xs.(j)) order;
  let group = Array.make n 0
  and u = Array.make n 0.
  and m = Array.make n 0.
  and g = ref 0 in
  Array.iter
    (fun i ->
      if !g = 0 || u.(!g - 1) <> xs.(i) then begin
        u.(!g) <- xs.(i);
        incr g
      end;
      m.(!g - 1) <- m.(!g - 1) +. w.(i);
      group.(i) <- !g - 1)
    order;
  let g = !g in
  let p0 = Array.make (g + 1) 0.
  and p1 = Array.make (g + 1) 0.
  and p2 = Array.make (g + 1) 0. in
  for i = 0 to g - 1 do
    p0.(i + 1) <- p0.(i) +. m.(i);
    p1.(i + 1) <- p1.(i) +. (m.(i) *. u.(i));
    p2.(i + 1) <- p2.(i) +. (m.(i) *. u.(i) *. u.(i))
  done;
  let cost i j =
    let s0 = p0.(j + 1) -. p0.(i) and s1 = p1.(j + 1) -. p1.(i) in
    Float.max 0. (p2.(j + 1) -. p2.(i) -. (s1 *. s1 /. s0))
  in
  let k = min k g in
  let best = Array.make_matrix k g infinity and cut = Array.make_matrix k g 0 in
  for j = 0 to g - 1 do
    best.(0).(j) <- cost 0 j
  done;
  for q = 1 to k - 1 do
    for j = q to g - 1 do
      for i = q to j do
        let c = best.(q - 1).(i - 1) +. cost i j in
        if c < best.(q).(j) then begin
          best.(q).(j) <- c;
          cut.(q).(j) <- i
        end
      done
    done
  done;
  let label = Array.make g 0 in
  let rec back q j =
    if q >= 0 && j >= 0 then begin
      let i = if q = 0 then 0 else cut.(q).(j) in
      for x = i to j do
        label.(x) <- q
      done;
      back (q - 1) (i - 1)
    end
  in
  if g > 0 then back (k - 1) (g - 1);
  Array.map (fun gi -> label.(gi)) group

let cdf h =
  let c = Array.copy h in
  for i = 1 to Array.length c - 1 do
    c.(i) <- c.(i - 1) +. c.(i)
  done;
  c

let mass c = Array.mapi (fun i x -> if i = 0 then x else x -. c.(i - 1)) c

let l1 a b =
  let s = ref 0. in
  for i = 0 to Array.length a - 1 do
    s := !s +. Float.abs (a.(i) -. b.(i))
  done;
  !s /. float_of_int (Array.length a)

let emd a b =
  if Array.length a <> Array.length b then
    invalid_arg "Cluster.emd: sizes differ";
  l1 (cdf a) (cdf b)

let median pairs =
  let pairs = Array.copy pairs in
  Array.sort (fun (x, _) (y, _) -> compare x y) pairs;
  let total = Array.fold_left (fun acc (_, w) -> acc +. w) 0. pairs in
  let rec go i acc =
    let x, w = pairs.(i) in
    if acc +. w >= total /. 2. || i = Array.length pairs - 1 then x
    else go (i + 1) (acc +. w)
  in
  go 0 0.

let medians w cdfs labels k old =
  Array.init k (fun l ->
      let members =
        List.filter
          (fun i -> labels.(i) = l)
          (List.init (Array.length cdfs) Fun.id)
      in
      if members = [] then old.(l)
      else
        Array.init
          (Array.length cdfs.(0))
          (fun b ->
            median
              (Array.of_list
                 (List.map (fun i -> (cdfs.(i).(b), w.(i))) members))))

let nearest centers x =
  let best = ref 0 and dist = ref (l1 centers.(0) x) in
  for l = 1 to Array.length centers - 1 do
    let e = l1 centers.(l) x in
    if e < !dist then begin
      best := l;
      dist := e
    end
  done;
  (!best, !dist)

let uniform rng = float_of_int (Rng.bits rng) *. 0x1p-30

let draw rng w =
  let total = Array.fold_left ( +. ) 0. w in
  let x = ref (uniform rng *. total) and i = ref 0 in
  while !i < Array.length w - 1 && !x >= w.(!i) do
    x := !x -. w.(!i);
    incr i
  done;
  !i

let seed_centers rng w cdfs k =
  let first = cdfs.(draw rng w) in
  let rec grow centers =
    if List.length centers = k then centers
    else
      let cs = Array.of_list centers in
      let d = Array.mapi (fun i x -> w.(i) *. snd (nearest cs x)) cdfs in
      if Array.for_all (fun x -> x = 0.) d then centers
      else grow (centers @ [ cdfs.(draw rng d) ])
  in
  Array.of_list (grow [ first ])

let lloyd w cdfs centers =
  let rec go centers labels steps =
    let next = Array.map (fun x -> fst (nearest centers x)) cdfs in
    if next = labels || steps = 100 then (centers, labels)
    else go (medians w cdfs next (Array.length centers) centers) next (steps + 1)
  in
  go centers (Array.make (Array.length cdfs) (-1)) 0

let cost w cdfs centers labels =
  let s = ref 0. in
  Array.iteri (fun i x -> s := !s +. (w.(i) *. l1 centers.(labels.(i)) x)) cdfs;
  !s

let by_mean centers labels =
  let k = Array.length centers in
  let means = Array.map (fun c -> -.Array.fold_left ( +. ) 0. c) centers in
  let order = Array.init k Fun.id in
  Array.stable_sort (fun a b -> compare means.(a) means.(b)) order;
  let rank = Array.make k 0 in
  Array.iteri (fun r l -> rank.(l) <- r) order;
  Array.map (fun l -> rank.(l)) labels

let kmedians ?weights ?(restarts = 10) rng hists k =
  if k < 1 then invalid_arg "Cluster.kmedians: k must be positive";
  let n = Array.length hists in
  let w = check_weights n weights in
  if n = 0 then [||]
  else
    let cdfs = Array.map cdf hists in
    let attempt () =
      let centers, labels = lloyd w cdfs (seed_centers rng w cdfs k) in
      (cost w cdfs centers labels, centers, labels)
    in
    let best = ref (attempt ()) in
    for _ = 2 to restarts do
      let ((c, _, _) as r) = attempt () and b, _, _ = !best in
      if c < b then best := r
    done;
    let _, centers, labels = !best in
    by_mean centers labels

let centers ?weights hists labels =
  let w = check_weights (Array.length hists) weights in
  let k = Array.fold_left max (-1) labels + 1 in
  let cdfs = Array.map cdf hists in
  Array.map mass (medians w cdfs labels k (Array.make k [||]))

let emd_cost ?weights hists labels =
  let w = check_weights (Array.length hists) weights in
  let cs = Array.map cdf (centers ~weights:w hists labels) in
  cost w (Array.map cdf hists) cs labels
