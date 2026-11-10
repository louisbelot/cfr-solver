type 'a t = {
  capacity : int;
  rng : Rng.t;
  mutable seen : int;
  mutable items : 'a array;
}

let create ~capacity rng =
  if capacity < 1 then invalid_arg "Reservoir.create: capacity";
  { capacity; rng; seen = 0; items = [||] }

let length r = min r.seen r.capacity
let seen r = r.seen

let add r x =
  if r.seen = 0 then r.items <- Array.make r.capacity x
  else if r.seen < r.capacity then r.items.(r.seen) <- x
  else begin
    let j = Rng.int r.rng (r.seen + 1) in
    if j < r.capacity then r.items.(j) <- x
  end;
  r.seen <- r.seen + 1

let get r i =
  if i < 0 || i >= length r then invalid_arg "Reservoir.get";
  r.items.(i)

let to_list r = List.init (length r) (fun i -> r.items.(i))
