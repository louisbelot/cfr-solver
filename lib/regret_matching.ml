let uniform_into out =
  let n = Array.length out in
  Array.fill out 0 n (1. /. float_of_int n)

let normalize_into weights out =
  let total = Array.fold_left ( +. ) 0. weights in
  if total > 0. then Array.iteri (fun a w -> out.(a) <- w /. total) weights
  else uniform_into out

let normalize weights =
  let out = Array.make (Array.length weights) 0. in
  normalize_into weights out;
  out

let strategy_into regrets out =
  let total = ref 0. in
  for a = 0 to Array.length regrets - 1 do
    total := !total +. Float.max regrets.(a) 0.
  done;
  let total = !total in
  if total > 0. then
    for a = 0 to Array.length regrets - 1 do
      out.(a) <- Float.max regrets.(a) 0. /. total
    done
  else uniform_into out

let strategy regrets =
  let out = Array.make (Array.length regrets) 0. in
  strategy_into regrets out;
  out
