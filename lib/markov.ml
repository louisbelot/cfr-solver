let closure p =
  let n = Array.length p in
  let r =
    Array.init n (fun i -> Array.init n (fun j -> i = j || p.(i).(j) > 0.))
  in
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      if r.(i).(k) then
        for j = 0 to n - 1 do
          if r.(k).(j) then r.(i).(j) <- true
        done
    done
  done;
  r

let solve a b =
  let n = Array.length b in
  for c = 0 to n - 1 do
    let piv = ref c in
    for i = c + 1 to n - 1 do
      if Float.abs a.(i).(c) > Float.abs a.(!piv).(c) then piv := i
    done;
    let t = a.(c) in
    a.(c) <- a.(!piv);
    a.(!piv) <- t;
    let t = b.(c) in
    b.(c) <- b.(!piv);
    b.(!piv) <- t;
    for i = c + 1 to n - 1 do
      let f = a.(i).(c) /. a.(c).(c) in
      if f <> 0. then begin
        for j = c to n - 1 do
          a.(i).(j) <- a.(i).(j) -. (f *. a.(c).(j))
        done;
        b.(i) <- b.(i) -. (f *. b.(c))
      end
    done
  done;
  let x = Array.make n 0. in
  for i = n - 1 downto 0 do
    let s = ref b.(i) in
    for j = i + 1 to n - 1 do
      s := !s -. (a.(i).(j) *. x.(j))
    done;
    x.(i) <- !s /. a.(i).(i)
  done;
  x

let stationary p =
  let n = Array.length p in
  let r = closure p in
  let recurrent i =
    let ok = ref true in
    for j = 0 to n - 1 do
      if r.(i).(j) && not r.(j).(i) then ok := false
    done;
    !ok
  in
  let first = ref 0 in
  while not (recurrent !first) do
    incr first
  done;
  let cls = List.filter (fun j -> r.(!first).(j)) (List.init n Fun.id) in
  let cls = Array.of_list cls in
  let m = Array.length cls in
  let a =
    Array.init m (fun i ->
        Array.init m (fun j ->
            if i = m - 1 then 1.
            else p.(cls.(j)).(cls.(i)) -. if i = j then 1. else 0.))
  in
  let b = Array.init m (fun i -> if i = m - 1 then 1. else 0.) in
  let x = solve a b in
  let pi = Array.make n 0. in
  Array.iteri (fun k j -> pi.(j) <- Float.max 0. x.(k)) cls;
  let s = Array.fold_left ( +. ) 0. pi in
  Array.map (fun v -> v /. s) pi
