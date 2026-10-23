type t = {
  lock : Mutex.t;
  cond : Condition.t;
  parties : int;
  mutable waiting : int;
  mutable generation : int;
}

let create parties =
  if parties < 1 then invalid_arg "Barrier.create: parties must be positive";
  {
    lock = Mutex.create ();
    cond = Condition.create ();
    parties;
    waiting = 0;
    generation = 0;
  }

let await b =
  Mutex.lock b.lock;
  let g = b.generation in
  b.waiting <- b.waiting + 1;
  if b.waiting = b.parties then begin
    b.waiting <- 0;
    b.generation <- g + 1;
    Condition.broadcast b.cond
  end
  else
    while b.generation = g do
      Condition.wait b.cond b.lock
    done;
  Mutex.unlock b.lock
