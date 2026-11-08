module Z = Bigint

type t = { num : Z.t; den : Z.t }

let zero = { num = Z.zero; den = Z.one }
let one = { num = Z.one; den = Z.one }
let is_one z = Z.equal z Z.one

let make n d =
  if Z.sign d = 0 then raise Division_by_zero;
  if Z.sign n = 0 then zero
  else
    let g = Z.gcd n d in
    let n, d = if is_one g then (n, d) else (Z.div n g, Z.div d g) in
    if Z.sign d < 0 then { num = Z.neg n; den = Z.neg d }
    else { num = n; den = d }

let of_int n = { num = Z.of_int n; den = Z.one }
let of_ints n d = make (Z.of_int n) (Z.of_int d)

let of_float x =
  if not (Float.is_finite x) then invalid_arg "Rat.of_float";
  let m, e = Float.frexp x in
  let n = Z.of_float (Float.ldexp m 53) and e = e - 53 in
  if e >= 0 then { num = Z.shift_left n e; den = Z.one }
  else make n (Z.shift_left Z.one (-e))

let recover ?(max_den = 1_000_000) x =
  if not (Float.is_finite x) then invalid_arg "Rat.recover";
  let tol = 1e-12 *. Float.max 1. (Float.abs x) in
  let rec go y h0 h1 k0 k1 =
    let a = Float.floor y in
    let h = (a *. h1) +. h0 and k = (a *. k1) +. k0 in
    if k > Float.of_int max_den then
      invalid_arg "Rat.recover: no small fraction"
    else if Float.abs (x -. (h /. k)) <= tol then
      make (Z.of_float h) (Z.of_float k)
    else go (1. /. (y -. a)) h1 h k1 k
  in
  go x 0. 1. 1. 0.

let sign q = Z.sign q.num
let is_zero q = Z.sign q.num = 0
let neg q = { q with num = Z.neg q.num }

let inv q =
  match Z.sign q.num with
  | 0 -> raise Division_by_zero
  | 1 -> { num = q.den; den = q.num }
  | _ -> { num = Z.neg q.den; den = Z.neg q.num }

let add a b =
  if is_zero a then b
  else if is_zero b then a
  else
    let g = Z.gcd a.den b.den in
    if is_one g then
      {
        num = Z.add (Z.mul a.num b.den) (Z.mul b.num a.den);
        den = Z.mul a.den b.den;
      }
    else
      let da = Z.div a.den g and db = Z.div b.den g in
      let t = Z.add (Z.mul a.num db) (Z.mul b.num da) in
      if Z.sign t = 0 then zero
      else
        let g' = Z.gcd t g in
        if is_one g' then { num = t; den = Z.mul da b.den }
        else { num = Z.div t g'; den = Z.mul da (Z.div b.den g') }

let sub a b = add a (neg b)

let mul a b =
  if is_zero a || is_zero b then zero
  else
    let g1 = Z.gcd a.num b.den and g2 = Z.gcd b.num a.den in
    let cut x g = if is_one g then x else Z.div x g in
    {
      num = Z.mul (cut a.num g1) (cut b.num g2);
      den = Z.mul (cut a.den g2) (cut b.den g1);
    }

let div a b = mul a (inv b)

let compare a b =
  if Z.equal a.den b.den then Z.compare a.num b.num
  else Z.compare (Z.mul a.num b.den) (Z.mul b.num a.den)

let equal a b = Z.equal a.num b.num && Z.equal a.den b.den
let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b

let to_float q =
  if is_zero q then 0.
  else
    let scale z =
      let k = Z.numbits z - 64 in
      (Z.to_float (if k > 0 then Z.shift_right z k else Z.shift_left z (-k)), k)
    in
    let n, a = scale q.num and d, b = scale q.den in
    Float.ldexp (n /. d) (a - b)

let to_string q =
  if is_one q.den then Z.to_string q.num
  else Z.to_string q.num ^ "/" ^ Z.to_string q.den

let pp ppf q = Format.pp_print_string ppf (to_string q)
