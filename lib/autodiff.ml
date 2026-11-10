type t = { rows : int; cols : int; v : float array; g : float array }

type tape = {
  grad : bool;
  mutable back : (unit -> unit) list;
  mutable arena : float array array;
  mutable used : int;
}

let tape () = { grad = true; back = []; arena = [||]; used = 0 }
let inference () = { grad = false; back = []; arena = [||]; used = 0 }

let reset tape =
  tape.back <- [];
  tape.used <- 0

let buffer tape n =
  let k = tape.used in
  tape.used <- k + 1;
  if k = Array.length tape.arena then
    tape.arena <- Array.append tape.arena (Array.make (max 8 k) [||]);
  let b = tape.arena.(k) in
  if Array.length b = n then begin
    Array.fill b 0 n 0.;
    b
  end
  else begin
    let b = Array.make n 0. in
    tape.arena.(k) <- b;
    b
  end

let node tape rows cols =
  let n = rows * cols in
  {
    rows;
    cols;
    v = buffer tape n;
    g = (if tape.grad then buffer tape n else [||]);
  }

let param rows cols v =
  if Array.length v <> rows * cols then invalid_arg "Autodiff.param: size";
  { rows; cols; v; g = Array.make (rows * cols) 0. }

let const rows cols v =
  if Array.length v <> rows * cols then invalid_arg "Autodiff.const: size";
  { rows; cols; v; g = [||] }

let shadow x = { x with g = Array.make (Array.length x.v) 0. }
let zero_grad x = Array.fill x.g 0 (Array.length x.g) 0.
let wants x = Array.length x.g > 0
let record tape f = if tape.grad then tape.back <- f :: tape.back

let matmul tape a b =
  if a.cols <> b.rows then invalid_arg "Autodiff.matmul: shapes";
  let n = a.rows and k = a.cols and m = b.cols in
  let c = node tape n m in
  let av = a.v and bv = b.v and cv = c.v in
  for i = 0 to n - 1 do
    for p = 0 to k - 1 do
      let x = Array.unsafe_get av ((i * k) + p) in
      if x <> 0. then begin
        let row = p * m and out = i * m in
        for j = 0 to m - 1 do
          Array.unsafe_set cv (out + j)
            (Array.unsafe_get cv (out + j)
            +. (x *. Array.unsafe_get bv (row + j)))
        done
      end
    done
  done;
  record tape (fun () ->
      let ag = a.g and bg = b.g and cg = c.g in
      let da = wants a and db = wants b in
      for i = 0 to n - 1 do
        for p = 0 to k - 1 do
          let x = Array.unsafe_get av ((i * k) + p) in
          let row = p * m and out = i * m in
          if da then begin
            let s = ref 0. in
            for j = 0 to m - 1 do
              s :=
                !s
                +. Array.unsafe_get cg (out + j)
                   *. Array.unsafe_get bv (row + j)
            done;
            Array.unsafe_set ag
              ((i * k) + p)
              (Array.unsafe_get ag ((i * k) + p) +. !s)
          end;
          if db && x <> 0. then
            for j = 0 to m - 1 do
              Array.unsafe_set bg (row + j)
                (Array.unsafe_get bg (row + j)
                +. (x *. Array.unsafe_get cg (out + j)))
            done
        done
      done);
  c

let add_bias tape x b =
  if b.rows <> 1 || b.cols <> x.cols then invalid_arg "Autodiff.add_bias";
  let m = x.cols in
  let y = node tape x.rows m in
  for i = 0 to (x.rows * m) - 1 do
    y.v.(i) <- x.v.(i) +. b.v.(i mod m)
  done;
  record tape (fun () ->
      for i = 0 to (x.rows * m) - 1 do
        let d = y.g.(i) in
        if wants x then x.g.(i) <- x.g.(i) +. d;
        if wants b then b.g.(i mod m) <- b.g.(i mod m) +. d
      done);
  y

let relu tape x =
  let n = x.rows * x.cols in
  let y = node tape x.rows x.cols in
  for i = 0 to n - 1 do
    y.v.(i) <- Float.max x.v.(i) 0.
  done;
  record tape (fun () ->
      if wants x then
        for i = 0 to n - 1 do
          if x.v.(i) > 0. then x.g.(i) <- x.g.(i) +. y.g.(i)
        done);
  y

let check_loss name x ~target ~mask ~weight =
  let n = x.rows * x.cols in
  if Array.length target <> n || Array.length mask <> n then
    invalid_arg (name ^ ": target or mask size");
  if Array.length weight <> x.rows then invalid_arg (name ^ ": weight size");
  let total = Array.fold_left ( +. ) 0. weight in
  if not (total > 0.) then invalid_arg (name ^ ": weights must sum above 0");
  total

let mse tape x ~target ~mask ~weight =
  let total = check_loss "Autodiff.mse" x ~target ~mask ~weight in
  let m = x.cols and n = x.rows * x.cols in
  let loss = ref 0. in
  for i = 0 to n - 1 do
    let e = x.v.(i) -. target.(i) in
    loss := !loss +. (weight.(i / m) *. mask.(i) *. e *. e)
  done;
  let l = node tape 1 1 in
  l.v.(0) <- !loss /. total;
  record tape (fun () ->
      if wants x then
        for i = 0 to n - 1 do
          let w = weight.(i / m) *. mask.(i) /. total in
          x.g.(i) <- x.g.(i) +. (l.g.(0) *. 2. *. w *. (x.v.(i) -. target.(i)))
        done);
  l

let softmax ~mask x =
  let m = x.cols in
  let p = Array.make (x.rows * m) 0. in
  for i = 0 to x.rows - 1 do
    let hi = ref Float.neg_infinity in
    for j = 0 to m - 1 do
      if mask.((i * m) + j) > 0. then hi := Float.max !hi x.v.((i * m) + j)
    done;
    let z = ref 0. in
    for j = 0 to m - 1 do
      if mask.((i * m) + j) > 0. then begin
        let e = exp (x.v.((i * m) + j) -. !hi) in
        p.((i * m) + j) <- e;
        z := !z +. e
      end
    done;
    for j = 0 to m - 1 do
      if !z > 0. then p.((i * m) + j) <- p.((i * m) + j) /. !z
    done
  done;
  p

let cross_entropy tape x ~target ~mask ~weight =
  let total = check_loss "Autodiff.cross_entropy" x ~target ~mask ~weight in
  let m = x.cols and p = softmax ~mask x in
  let loss = ref 0. in
  for i = 0 to (x.rows * m) - 1 do
    let q = target.(i) in
    if q > 0. then loss := !loss -. (weight.(i / m) *. q *. log p.(i))
  done;
  let l = node tape 1 1 in
  l.v.(0) <- !loss /. total;
  record tape (fun () ->
      if wants x then
        for i = 0 to x.rows - 1 do
          let w = weight.(i) /. total and s = ref 0. in
          for j = 0 to m - 1 do
            s := !s +. target.((i * m) + j)
          done;
          for j = 0 to m - 1 do
            let k = (i * m) + j in
            if mask.(k) > 0. then
              x.g.(k) <-
                x.g.(k) +. (l.g.(0) *. w *. ((!s *. p.(k)) -. target.(k)))
          done
        done);
  l

let backward tape loss =
  if loss.rows * loss.cols <> 1 then invalid_arg "Autodiff.backward: scalar";
  loss.g.(0) <- 1.;
  List.iter (fun f -> f ()) tape.back;
  tape.back <- []
