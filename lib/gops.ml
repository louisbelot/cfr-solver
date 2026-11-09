module type FIELD = sig
  include Lp.FIELD

  val matrix : t array array -> t array * t array * t
end

module type S = sig
  type field
  type t

  val create : Goofspiel.order -> t
  val size : t -> int
  val value : t -> int -> int -> int -> field
  val turn : t -> int -> int -> int -> int -> field array * field array * field
end

module Make (F : FIELD) = struct
  type field = F.t
  type t = { order : Goofspiel.order; memo : (int, F.t) Hashtbl.t }

  let create order = { order; memo = Hashtbl.create 4096 }
  let size t = Hashtbl.length t.memo
  let bit k = 1 lsl (k - 1)
  let key v y p = (v lsl 26) lor (y lsl 13) lor p
  let int k = F.of_rat (Rat.of_int k)

  let upcards order p =
    let ks = Goofspiel.members p in
    match order with
    | Goofspiel.Random -> ks
    | Ascending -> [ List.hd ks ]
    | Descending -> [ List.hd (List.rev ks) ]

  let rec value t v y p =
    if p = 0 || v = y then F.zero
    else
      match Hashtbl.find_opt t.memo (key v y p) with
      | Some w -> w
      | None -> (
          match Hashtbl.find_opt t.memo (key y v p) with
          | Some w -> F.sub F.zero w
          | None ->
              let ks = upcards t.order p in
              let total =
                List.fold_left
                  (fun acc k ->
                    let _, _, w = turn t v y p k in
                    F.add acc w)
                  F.zero ks
              in
              let w = F.div total (int (List.length ks)) in
              Hashtbl.add t.memo (key v y p) w;
              w)

  and turn t v y p k =
    let rest = p land lnot (bit k) and ys = Goofspiel.members y in
    Goofspiel.members v
    |> List.map (fun a ->
        List.map
          (fun b ->
            F.add
              (int (k * compare a b))
              (value t (v land lnot (bit a)) (y land lnot (bit b)) rest))
          ys
        |> Array.of_list)
    |> Array.of_list |> F.matrix
end

module Exact = Make (struct
  include Lp.Exact

  let matrix = Meta_game.matrix
end)

module Tableau =
  Meta_lp.Make
    (struct
      include Lp.Approx

      let sub = ( -. )
      let sign x = if x > 1e-9 then 1 else if x < -1e-9 then -1 else 0
    end)
    (struct
      let delta = 1e-9
    end)

let normalize v =
  let v = Array.map (Float.max 0.) v in
  let total = Array.fold_left ( +. ) 0. v in
  Array.map (fun x -> x /. total) v

let float_matrix a =
  let lo = Array.fold_left (Array.fold_left Float.min) infinity a in
  let t = Tableau.create ~shift:(1. -. lo) in
  Tableau.add_row t [||];
  Array.iter (fun x -> Tableau.add_col t [| x |]) a.(0);
  Array.iteri (fun i row -> if i > 0 then Tableau.add_row t row) a;
  let read () =
    let x, y, v = Tableau.solution t in
    let x = normalize x and y = normalize y in
    (Tableau.gap t (x, y), (x, y, v))
  in
  let g, sol = read () in
  if g <= 1e-10 then sol
  else begin
    Tableau.refactor t;
    snd (read ())
  end

module Float = Make (struct
  include Lp.Approx

  let sub = ( -. )
  let matrix = float_matrix
end)

let strategy (c : Goofspiel.config) =
  let t = Float.create c.order in
  Goofspiel.profile c (fun ~mine ~theirs ~deck ~upcard ->
      let x, y, _ = Float.turn t mine theirs deck upcard in
      (x, y))
