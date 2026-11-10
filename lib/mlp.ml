type layer = { w : Autodiff.t; b : Autodiff.t }
type t = { sizes : int array; layers : layer array }

let create rng sizes =
  let sizes = Array.of_list sizes in
  if Array.length sizes < 2 || Array.exists (fun n -> n < 1) sizes then
    invalid_arg "Mlp.create: need at least input and output sizes";
  let layers =
    Array.init
      (Array.length sizes - 1)
      (fun l ->
        let n = sizes.(l) and m = sizes.(l + 1) in
        let a = sqrt (6. /. float_of_int n) in
        let w =
          Array.init (n * m) (fun _ -> a *. ((2. *. Rng.float rng) -. 1.))
        in
        { w = Autodiff.param n m w; b = Autodiff.param 1 m (Array.make m 0.) })
  in
  { sizes; layers }

let inputs net = net.sizes.(0)

let params net =
  Array.to_list net.layers |> List.concat_map (fun l -> [ l.w; l.b ])

let replica net =
  {
    net with
    layers =
      Array.map
        (fun { w; b } -> { w = Autodiff.shadow w; b = Autodiff.shadow b })
        net.layers;
  }

let gather net ~scale replica =
  List.iter2
    (fun (p : Autodiff.t) (r : Autodiff.t) ->
      for i = 0 to Array.length r.g - 1 do
        p.g.(i) <- p.g.(i) +. (scale *. r.g.(i))
      done;
      Autodiff.zero_grad r)
    (params net) (params replica)

let size net =
  List.fold_left (fun n (p : Autodiff.t) -> n + Array.length p.v) 0 (params net)

let forward tape net (x : Autodiff.t) =
  if x.cols <> inputs net then invalid_arg "Mlp.forward: input width";
  let last = Array.length net.layers - 1 in
  let h = ref x in
  Array.iteri
    (fun l { w; b } ->
      let z = Autodiff.add_bias tape (Autodiff.matmul tape !h w) b in
      h := if l = last then z else Autodiff.relu tape z)
    net.layers;
  !h

let predict net ~rows x =
  (forward (Autodiff.inference ()) net (Autodiff.const rows (inputs net) x)).v

type adam = {
  lr : float;
  beta1 : float;
  beta2 : float;
  eps : float;
  clip : float option;
  mutable step : int;
  m : float array array;
  s : float array array;
}

let adam ?(lr = 1e-3) ?(beta1 = 0.9) ?(beta2 = 0.999) ?(eps = 1e-8) ?clip net =
  let zeros () =
    params net
    |> List.map (fun (p : Autodiff.t) -> Array.make (Array.length p.v) 0.)
    |> Array.of_list
  in
  { lr; beta1; beta2; eps; clip; step = 0; m = zeros (); s = zeros () }

let grad_norm net =
  let sq = ref 0. in
  List.iter
    (fun (p : Autodiff.t) ->
      for i = 0 to Array.length p.g - 1 do
        sq := !sq +. (p.g.(i) *. p.g.(i))
      done)
    (params net);
  sqrt !sq

let update opt net =
  opt.step <- opt.step + 1;
  let scale =
    match opt.clip with
    | Some c ->
        let n = grad_norm net in
        if n > c then c /. n else 1.
    | None -> 1.
  in
  let t = float_of_int opt.step in
  let c1 = 1. -. (opt.beta1 ** t) and c2 = 1. -. (opt.beta2 ** t) in
  List.iteri
    (fun k (p : Autodiff.t) ->
      let m = opt.m.(k) and s = opt.s.(k) in
      for i = 0 to Array.length p.g - 1 do
        let g = p.g.(i) *. scale in
        m.(i) <- (opt.beta1 *. m.(i)) +. ((1. -. opt.beta1) *. g);
        s.(i) <- (opt.beta2 *. s.(i)) +. ((1. -. opt.beta2) *. g *. g);
        p.v.(i) <-
          p.v.(i) -. (opt.lr *. (m.(i) /. c1) /. (sqrt (s.(i) /. c2) +. opt.eps))
      done;
      Autodiff.zero_grad p)
    (params net)
