type t = { sizes : int array; u : float array array }

let rows m n entry =
  let cols =
    List.init n (fun j ->
        {
          Lp.coeffs =
            (m, Rat.neg Rat.one) :: List.init m (fun i -> (i, entry i j));
          rel = Ge;
          rhs = Rat.zero;
        })
  and sum =
    { Lp.coeffs = List.init m (fun i -> (i, Rat.one)); rel = Eq; rhs = Rat.one }
  in
  sum :: cols

let maximin a =
  let m = Array.length a and n = Array.length a.(0) in
  let shift =
    Rat.sub Rat.one (Array.fold_left (Array.fold_left Rat.min) a.(0).(0) a)
  in
  match
    Lp.maximize ~vars:(m + 1)
      [ (m, Rat.one) ]
      (rows m n (fun i j -> Rat.add a.(i).(j) shift))
  with
  | Optimal { value; x; _ } -> (Array.sub x 0 m, Rat.sub value shift)
  | Infeasible | Unbounded -> failwith "Meta_game.matrix: LP not optimal"

let matrix a =
  if Array.length a = 0 || Array.length a.(0) = 0 then
    invalid_arg "Meta_game.matrix: empty game";
  let x, v = maximin a
  and y, w =
    maximin
      (Array.init
         (Array.length a.(0))
         (fun j -> Array.init (Array.length a) (fun i -> Rat.neg a.(i).(j))))
  in
  if not (Rat.equal v (Rat.neg w)) then failwith "Meta_game.matrix: duality gap";
  (x, y, v)

let decode sizes idx =
  let n = Array.length sizes in
  let s = Array.make n 0 and r = ref idx in
  for p = n - 1 downto 0 do
    s.(p) <- !r mod sizes.(p);
    r := !r / sizes.(p)
  done;
  s

let count sizes = Array.fold_left ( * ) 1 sizes

let weight w s skip =
  let x = ref 1. in
  Array.iteri (fun q i -> if q <> skip then x := !x *. w.(q).(i)) s;
  !x

let profiles sizes f =
  let n = Array.length sizes in
  let s = Array.make n 0 in
  for idx = 0 to count sizes - 1 do
    f idx s;
    let p = ref (n - 1) in
    while !p >= 0 && s.(!p) = sizes.(!p) - 1 do
      s.(!p) <- 0;
      decr p
    done;
    if !p >= 0 then s.(!p) <- s.(!p) + 1
  done

let expected t w =
  let v = Array.make (Array.length t.sizes) 0. in
  profiles t.sizes (fun idx s ->
      let x = weight w s (-1) in
      Array.iteri (fun p up -> v.(p) <- v.(p) +. (x *. up)) t.u.(idx));
  v

let fitness t w =
  let f = Array.map (fun k -> Array.make k 0.) t.sizes in
  profiles t.sizes (fun idx s ->
      let u = t.u.(idx) in
      Array.iteri
        (fun p i -> f.(p).(i) <- f.(p).(i) +. (weight w s p *. u.(p)))
        s);
  f

let project floor x =
  let k = Array.length x in
  let z = 1. -. (float_of_int k *. floor) in
  let y = Array.map (fun v -> v -. floor) x in
  let sorted = Array.copy y in
  Array.sort (fun a b -> Float.compare b a) sorted;
  let theta = ref 0. and acc = ref 0. in
  Array.iteri
    (fun j v ->
      acc := !acc +. v;
      let t = (!acc -. z) /. float_of_int (j + 1) in
      if v -. t > 0. then theta := t)
    sorted;
  Array.map (fun v -> Float.max 0. (v -. !theta) +. floor) y

let prd ?(iterations = 10_000) ?(dt = 1e-3) ?(gamma = 1e-6) t =
  let x =
    ref (Array.map (fun k -> Array.make k (1. /. float_of_int k)) t.sizes)
  in
  let avg = Array.map (fun k -> Array.make k 0.) t.sizes in
  for _ = 1 to iterations do
    let f = fitness t !x in
    x :=
      Array.mapi
        (fun p xp ->
          let mean = ref 0. in
          Array.iteri (fun i v -> mean := !mean +. (v *. f.(p).(i))) xp;
          project
            (gamma /. float_of_int (Array.length xp))
            (Array.mapi (fun i v -> v +. (dt *. v *. (f.(p).(i) -. !mean))) xp))
        !x;
    Array.iteri
      (fun p xp -> Array.iteri (fun i v -> avg.(p).(i) <- avg.(p).(i) +. v) xp)
      !x
  done;
  Array.map (Array.map (fun v -> v /. float_of_int iterations)) avg

let fixation ~alpha ~m delta =
  let m' = float_of_int m in
  if delta = 0. then 1. /. m'
  else
    let x = alpha *. Float.abs delta in
    let r = Float.expm1 (-.x) /. Float.expm1 (-.m' *. x) in
    if delta > 0. then r else Float.exp (-.(m' -. 1.) *. x) *. r

let solve a =
  let n = Array.length a in
  let b = Array.make n 0. in
  b.(n - 1) <- 1.;
  for c = 0 to n - 1 do
    let p = ref c in
    for r = c + 1 to n - 1 do
      if Float.abs a.(r).(c) > Float.abs a.(!p).(c) then p := r
    done;
    let tmp = a.(c) in
    a.(c) <- a.(!p);
    a.(!p) <- tmp;
    let tb = b.(c) in
    b.(c) <- b.(!p);
    b.(!p) <- tb;
    let pivot = a.(c).(c) in
    for r = c + 1 to n - 1 do
      let f = a.(r).(c) /. pivot in
      if f <> 0. then begin
        let ar = a.(r) and ac = a.(c) in
        for k = c to n - 1 do
          ar.(k) <- ar.(k) -. (f *. ac.(k))
        done;
        b.(r) <- b.(r) -. (f *. b.(c))
      end
    done
  done;
  let x = Array.make n 0. in
  for r = n - 1 downto 0 do
    let s = ref b.(r) in
    for k = r + 1 to n - 1 do
      s := !s -. (a.(r).(k) *. x.(k))
    done;
    x.(r) <- !s /. a.(r).(r)
  done;
  x

let alpharank ~alpha ~m t =
  let n = count t.sizes in
  let moves = Array.fold_left (fun acc k -> acc + k - 1) 0 t.sizes in
  if moves = 0 then Array.make n 1.
  else begin
    let eta = 1. /. float_of_int moves in
    let a = Array.init n (fun _ -> Array.make n 0.) in
    let stride = Array.make (Array.length t.sizes) 1 in
    for p = Array.length t.sizes - 2 downto 0 do
      stride.(p) <- stride.(p + 1) * t.sizes.(p + 1)
    done;
    for i = 0 to n - 1 do
      let s = decode t.sizes i and out = ref 0. in
      Array.iteri
        (fun p sp ->
          for alt = 0 to t.sizes.(p) - 1 do
            if alt <> sp then begin
              let j = i + ((alt - sp) * stride.(p)) in
              let c = eta *. fixation ~alpha ~m (t.u.(j).(p) -. t.u.(i).(p)) in
              a.(j).(i) <- a.(j).(i) +. c;
              out := !out +. c
            end
          done)
        s;
      a.(i).(i) <- a.(i).(i) -. !out
    done;
    a.(n - 1) <- Array.make n 1.;
    Array.map (Float.max 0.) (solve a)
  end

let marginals t pi =
  let w = Array.map (fun k -> Array.make k 0.) t.sizes in
  profiles t.sizes (fun idx s ->
      Array.iteri (fun p i -> w.(p).(i) <- w.(p).(i) +. pi.(idx)) s);
  w
