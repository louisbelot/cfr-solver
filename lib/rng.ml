type t = Bytes.t

let of_state s =
  let b = Bytes.make 64 '\000' in
  Bytes.set_int64_le b 0 s;
  b

let make seed = of_state (Int64.of_int seed)
let copy = Bytes.copy

let[@inline] next64 b =
  let s = Int64.add (Bytes.get_int64_le b 0) 0x9e3779b97f4a7c15L in
  Bytes.set_int64_le b 0 s;
  let z =
    Int64.(mul (logxor s (shift_right_logical s 30)) 0xbf58476d1ce4e5b9L)
  in
  let z =
    Int64.(mul (logxor z (shift_right_logical z 27)) 0x94d049bb133111ebL)
  in
  Int64.(logxor z (shift_right_logical z 31))

let split b = of_state (next64 b)
let[@inline] bits b = Int64.to_int (Int64.shift_right_logical (next64 b) 34)
let[@inline] int b n = (bits b * n) lsr 30
let[@inline] float b = float_of_int (bits b) *. 0x1p-30

let draw rng w =
  let total = Array.fold_left ( +. ) 0. w in
  let x = ref (float rng *. total) and i = ref 0 in
  while !i < Array.length w - 1 && !x >= w.(!i) do
    x := !x -. w.(!i);
    incr i
  done;
  !i

let pick rng probs base n =
  let x = ref (float rng) and a = ref 0 in
  while !a < n - 1 && !x >= probs.(base + !a) do
    x := !x -. probs.(base + !a);
    incr a
  done;
  !a
