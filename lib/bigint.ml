module type S = sig
  type t

  val zero : t
  val one : t
  val of_int : int -> t
  val of_float : float -> t
  val of_string : string -> t
  val to_string : t -> string
  val to_float : t -> float
  val sign : t -> int
  val neg : t -> t
  val abs : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div_rem : t -> t -> t * t
  val div : t -> t -> t
  val rem : t -> t -> t
  val gcd : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val numbits : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Make (L : sig
  val bits : int
end) =
struct
  let bits = L.bits

  let () =
    if bits < 2 || (2 * bits) + 2 > Sys.int_size then
      invalid_arg "Bigint.Make: limb width"

  let base = 1 lsl bits
  let mask = base - 1

  module Nat = struct
    let trim a =
      let n = ref (Array.length a) in
      while !n > 0 && a.(!n - 1) = 0 do
        decr n
      done;
      if !n = Array.length a then a else Array.sub a 0 !n

    let compare a b =
      let la = Array.length a and lb = Array.length b in
      if la <> lb then Int.compare la lb
      else
        let rec go i =
          if i < 0 then 0
          else if a.(i) <> b.(i) then Int.compare a.(i) b.(i)
          else go (i - 1)
        in
        go (la - 1)

    let add a b =
      let a, b = if Array.length a >= Array.length b then (a, b) else (b, a) in
      let la = Array.length a and lb = Array.length b in
      let r = Array.make (la + 1) 0 and carry = ref 0 in
      for i = 0 to la - 1 do
        let s = a.(i) + (if i < lb then b.(i) else 0) + !carry in
        r.(i) <- s land mask;
        carry := s lsr bits
      done;
      r.(la) <- !carry;
      trim r

    let sub a b =
      let la = Array.length a and lb = Array.length b in
      let r = Array.make la 0 and borrow = ref 0 in
      for i = 0 to la - 1 do
        let d = a.(i) - (if i < lb then b.(i) else 0) - !borrow in
        if d < 0 then (
          r.(i) <- d + base;
          borrow := 1)
        else (
          r.(i) <- d;
          borrow := 0)
      done;
      trim r

    let mul a b =
      let la = Array.length a and lb = Array.length b in
      if la = 0 || lb = 0 then [||]
      else
        let r = Array.make (la + lb) 0 in
        for i = 0 to la - 1 do
          let ai = a.(i) in
          if ai <> 0 then (
            let carry = ref 0 in
            for j = 0 to lb - 1 do
              let t = r.(i + j) + (ai * b.(j)) + !carry in
              r.(i + j) <- t land mask;
              carry := t lsr bits
            done;
            r.(i + lb) <- !carry)
        done;
        trim r

    let bit_length x =
      let rec go n x = if x = 0 then n else go (n + 1) (x lsr 1) in
      go 0 x

    let numbits a =
      let n = Array.length a in
      if n = 0 then 0 else ((n - 1) * bits) + bit_length a.(n - 1)

    let shift_left a k =
      let n = Array.length a in
      if n = 0 then a
      else
        let limbs = k / bits and s = k mod bits in
        let r = Array.make (n + limbs + 1) 0 in
        for i = 0 to n - 1 do
          let v = a.(i) lsl s in
          r.(i + limbs) <- r.(i + limbs) lor (v land mask);
          r.(i + limbs + 1) <- v lsr bits
        done;
        trim r

    let shift_right a k =
      let n = Array.length a and limbs = k / bits and s = k mod bits in
      if limbs >= n then [||]
      else
        let r = Array.make (n - limbs) 0 in
        for i = 0 to n - limbs - 1 do
          let hi =
            if i + limbs + 1 < n then
              (a.(i + limbs + 1) lsl (bits - s)) land mask
            else 0
          in
          r.(i) <- (a.(i + limbs) lsr s) lor hi
        done;
        trim r

    let div_small a d =
      let n = Array.length a in
      let q = Array.make n 0 and r = ref 0 in
      for i = n - 1 downto 0 do
        let cur = (!r lsl bits) lor a.(i) in
        q.(i) <- cur / d;
        r := cur mod d
      done;
      (trim q, !r)

    let div_rem a b =
      let n = Array.length b in
      if n = 0 then raise Division_by_zero
      else if compare a b < 0 then ([||], a)
      else if n = 1 then
        let q, r = div_small a b.(0) in
        (q, if r = 0 then [||] else [| r |])
      else
        let s = bits - bit_length b.(n - 1) in
        let v = shift_left b s and u = Array.make (Array.length a + 1) 0 in
        let shifted = shift_left a s in
        Array.blit shifted 0 u 0 (Array.length shifted);
        let m = Array.length a - n in
        let q = Array.make (m + 1) 0 in
        let vt = v.(n - 1) and vs = v.(n - 2) in
        for j = m downto 0 do
          let num = (u.(j + n) lsl bits) lor u.(j + n - 1) in
          let qhat = ref (num / vt) and rhat = ref (num mod vt) in
          while
            !rhat < base
            && (!qhat >= base || !qhat * vs > (!rhat lsl bits) lor u.(j + n - 2))
          do
            decr qhat;
            rhat := !rhat + vt
          done;
          let borrow = ref 0 and carry = ref 0 in
          for i = 0 to n - 1 do
            let p = (!qhat * v.(i)) + !carry in
            carry := p lsr bits;
            let t = u.(i + j) - (p land mask) - !borrow in
            if t < 0 then (
              u.(i + j) <- t + base;
              borrow := 1)
            else (
              u.(i + j) <- t;
              borrow := 0)
          done;
          let t = u.(j + n) - !carry - !borrow in
          if t >= 0 then (
            u.(j + n) <- t;
            q.(j) <- !qhat)
          else (
            q.(j) <- !qhat - 1;
            let c = ref 0 in
            for i = 0 to n - 1 do
              let s = u.(i + j) + v.(i) + !c in
              u.(i + j) <- s land mask;
              c := s lsr bits
            done;
            u.(j + n) <- t + !c)
        done;
        (trim q, shift_right (trim (Array.sub u 0 n)) s)

    let rec gcd a b =
      if Array.length b = 0 then a else gcd b (snd (div_rem a b))
  end

  type t = S of int | L of { sign : int; mag : int array }

  let limit = base * base

  let small n =
    if n = 0 then [||]
    else if n < base then [| n |]
    else [| n land mask; n lsr bits |]

  let norm sign mag =
    match Array.length mag with
    | 0 -> S 0
    | 1 -> S (sign * mag.(0))
    | 2 -> S (sign * (mag.(0) lor (mag.(1) lsl bits)))
    | _ -> L { sign; mag }

  let parts = function
    | S n -> (Int.compare n 0, small (Int.abs n))
    | L { sign; mag } -> (sign, mag)

  let zero = S 0
  let one = S 1

  let of_int n =
    if n > -limit && n < limit then S n
    else
      let rec limbs n acc =
        if n = 0 then Array.of_list (List.rev acc)
        else limbs (n / base) (-(n mod base) :: acc)
      in
      norm (Int.compare n 0) (limbs (if n > 0 then -n else n) [])

  let of_float f =
    if not (Float.is_integer f) then
      invalid_arg "Bigint.of_float: not an integer";
    if Float.abs f < Float.of_int limit then S (Float.to_int f)
    else
      let fb = Float.of_int base in
      let rec limbs f acc =
        if f = 0. then Array.of_list (List.rev acc)
        else
          let q = Float.trunc (f /. fb) in
          limbs q (Float.to_int (f -. (q *. fb)) :: acc)
      in
      norm (Float.compare f 0.) (limbs (Float.abs f) [])

  let sign = function S n -> Int.compare n 0 | L r -> r.sign
  let neg = function S n -> S (-n) | L r -> L { r with sign = -r.sign }
  let abs = function S n -> S (Int.abs n) | L r -> L { r with sign = 1 }

  let compare a b =
    match (a, b) with
    | S x, S y -> Int.compare x y
    | S _, L r -> -r.sign
    | L r, S _ -> r.sign
    | L r, L r' ->
        if r.sign <> r'.sign then Int.compare r.sign r'.sign
        else if r.sign > 0 then Nat.compare r.mag r'.mag
        else Nat.compare r'.mag r.mag

  let equal a b = compare a b = 0

  let add a b =
    match (a, b) with
    | S x, S y -> of_int (x + y)
    | _ ->
        let sa, ma = parts a and sb, mb = parts b in
        if sa = 0 then b
        else if sb = 0 then a
        else if sa = sb then norm sa (Nat.add ma mb)
        else
          let c = Nat.compare ma mb in
          if c = 0 then zero
          else if c > 0 then norm sa (Nat.sub ma mb)
          else norm sb (Nat.sub mb ma)

  let sub a b = add a (neg b)

  let mul a b =
    match (a, b) with
    | S x, S y when Int.abs x < base && Int.abs y < base -> S (x * y)
    | _ ->
        let sa, ma = parts a and sb, mb = parts b in
        norm (sa * sb) (Nat.mul ma mb)

  let div_rem a b =
    match (a, b) with
    | _, S 0 -> raise Division_by_zero
    | S x, S y -> (S (x / y), S (x mod y))
    | _ ->
        let sa, ma = parts a and sb, mb = parts b in
        let q, r = Nat.div_rem ma mb in
        (norm (sa * sb) q, norm sa r)

  let div a b = fst (div_rem a b)
  let rem a b = snd (div_rem a b)

  let gcd a b =
    match (a, b) with
    | S x, S y ->
        let rec go a b = if b = 0 then a else go b (a mod b) in
        S (go (Int.abs x) (Int.abs y))
    | _ -> norm 1 (Nat.gcd (snd (parts a)) (snd (parts b)))

  let shift_left a k =
    let s, m = parts a in
    norm s (Nat.shift_left m k)

  let shift_right a k =
    let s, m = parts a in
    norm s (Nat.shift_right m k)

  let numbits a = Nat.numbits (snd (parts a))

  let to_float = function
    | S n -> Float.of_int n
    | L r ->
        let fb = Float.of_int base in
        let m =
          Array.fold_right (fun l acc -> (acc *. fb) +. Float.of_int l) r.mag 0.
        in
        Float.of_int r.sign *. m

  let chunk, chunk_digits =
    let rec go p k = if p * 10 >= base then (p, k) else go (p * 10) (k + 1) in
    go 10 1

  let to_string = function
    | S n -> string_of_int n
    | L r ->
        let rec digits mag acc =
          if Array.length mag = 0 then acc
          else
            let q, d = Nat.div_small mag chunk in
            digits q (d :: acc)
        in
        let b = Buffer.create 32 in
        if r.sign < 0 then Buffer.add_char b '-';
        List.iteri
          (fun i d ->
            if i = 0 then Buffer.add_string b (string_of_int d)
            else Buffer.add_string b (Printf.sprintf "%0*d" chunk_digits d))
          (digits r.mag []);
        Buffer.contents b

  let of_string s =
    let n = String.length s in
    let negative = n > 0 && s.[0] = '-' in
    let start = if negative then 1 else 0 in
    if start = n then invalid_arg "Bigint.of_string";
    let ten = of_int 10 and acc = ref zero in
    for i = start to n - 1 do
      match s.[i] with
      | '0' .. '9' as c ->
          acc := add (mul !acc ten) (of_int (Char.code c - Char.code '0'))
      | _ -> invalid_arg "Bigint.of_string"
    done;
    if negative then neg !acc else !acc

  let pp ppf a = Format.pp_print_string ppf (to_string a)
end

include Make (struct
  let bits = (Sys.int_size - 2) / 2
end)
