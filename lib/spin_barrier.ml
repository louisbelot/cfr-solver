type t = { parties : int; count : int Atomic.t; generation : int Atomic.t }

let create parties =
  if parties < 1 then
    invalid_arg "Spin_barrier.create: parties must be positive";
  { parties; count = Atomic.make 0; generation = Atomic.make 0 }

let await b =
  let g = Atomic.get b.generation in
  if Atomic.fetch_and_add b.count 1 = b.parties - 1 then begin
    Atomic.set b.count 0;
    Atomic.incr b.generation
  end
  else
    while Atomic.get b.generation = g do
      Domain.cpu_relax ()
    done
