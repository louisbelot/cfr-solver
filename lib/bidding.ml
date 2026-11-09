type config = {
  bids : int;
  rule : Auction.rule;
  ties : Auction.ties;
  prior : ((int * int) * Rat.t) list;
  value : int -> int -> int -> Rat.t;
}

let normalize weights =
  let total =
    List.fold_left (fun acc (_, w) -> Rat.add acc w) Rat.zero weights
  in
  List.map (fun (s, w) -> (s, Rat.div w total)) weights

let grid x0 x1 f =
  List.concat_map
    (fun s0 -> List.init (x1 + 1) (fun s1 -> ((s0, s1), f s0 s1)))
    (List.init (x0 + 1) Fun.id)

let independent x0 x1 = normalize (grid x0 x1 (fun _ _ -> Rat.one))

let affiliated x =
  normalize (grid x x (fun s0 s1 -> Rat.of_int (x + (s0 * s1))))

let private_value i s0 s1 = Rat.of_int (if i = 0 then s0 else s1)
let wallet _ s0 s1 = Rat.of_int (s0 + s1)

let signals c i =
  List.sort_uniq compare
    (List.map (fun ((s0, s1), _) -> if i = 0 then s0 else s1) c.prior)

let key i s = Printf.sprintf "%d:%d" i s

let settle c (s0, s1) b0 b1 =
  let b = [| b0; b1 |] in
  let share i =
    if b.(i) > b.(1 - i) then Rat.one
    else if b.(i) < b.(1 - i) then Rat.zero
    else match c.ties with Split -> Rat.of_ints 1 2 | Void -> Rat.zero
  in
  let paid i =
    match c.rule with
    | All_pay -> Rat.of_int b.(i)
    | First_price -> Rat.mul (share i) (Rat.of_int b.(i))
    | Second_price -> Rat.mul (share i) (Rat.of_int b.(1 - i))
  in
  let u i = Rat.sub (Rat.mul (share i) (c.value i s0 s1)) (paid i) in
  (u 0, u 1, Rat.add (paid 0) (paid 1))

type history =
  | Deal
  | Drawn of int * int
  | Half of int * int * int
  | Done of int * int * int * int

let game c =
  let module G = struct
    type nonrec history = history

    let name = "bidding"
    let players = 2
    let root = Deal
    let moves f = List.init (c.bids + 1) (fun b -> (string_of_int b, f b))

    let node = function
      | Deal ->
          Multi.Chance
            (List.map
               (fun ((s0, s1), p) -> (Rat.to_float p, Drawn (s0, s1)))
               c.prior)
      | Drawn (s0, s1) ->
          Decision
            {
              player = 0;
              infoset = key 0 s0;
              actions = moves (fun b -> Half (s0, s1, b));
            }
      | Half (s0, s1, b0) ->
          Decision
            {
              player = 1;
              infoset = key 1 s1;
              actions = moves (fun b -> Done (s0, s1, b0, b));
            }
      | Done (s0, s1, b0, b1) ->
          let u0, u1, _ = settle c (s0, s1) b0 b1 in
          Terminal [| Rat.to_float u0; Rat.to_float u1 |]
  end in
  (module G : Multi.S with type history = history)

let outcome c sigma =
  List.fold_left
    (fun acc ((s0, s1), p) ->
      let q0 = sigma 0 s0 and q1 = sigma 1 s1 in
      let acc = ref acc in
      Array.iteri
        (fun b0 p0 ->
          if not (Rat.is_zero p0) then
            Array.iteri
              (fun b1 p1 ->
                if not (Rat.is_zero p1) then
                  let w = Rat.mul p (Rat.mul p0 p1) in
                  let u0, u1, r = settle c (s0, s1) b0 b1 in
                  let a, b, d = !acc in
                  acc :=
                    ( Rat.add a (Rat.mul w u0),
                      Rat.add b (Rat.mul w u1),
                      Rat.add d (Rat.mul w r) ))
              q1)
        q0;
      !acc)
    (Rat.zero, Rat.zero, Rat.zero)
    c.prior

let pure c f i s =
  Array.init (c.bids + 1) (fun b -> if b = f i s then Rat.one else Rat.zero)

let naive c i s =
  let mass, total =
    List.fold_left
      (fun (m, t) (((s0, s1) as d), p) ->
        if (if i = 0 then s0 else s1) = s then
          (Rat.add m p, Rat.add t (Rat.mul p (c.value i (fst d) (snd d))))
        else (m, t))
      (Rat.zero, Rat.zero) c.prior
  in
  let e = Rat.to_float (Rat.div total mass) in
  min c.bids (Float.to_int (Float.round e))

let equilibrium c =
  let t = Lemke.build (module (val game c)) in
  let sol = Lemke.solve t in
  let rows =
    Hashtbl.of_seq
      (List.to_seq
         (Lemke.behavior t 0 (fst sol.plans)
         @ Lemke.behavior t 1 (snd sol.plans)))
  in
  (t, sol, fun i s -> Hashtbl.find rows (key i s))
