type failure = exn * Printexc.raw_backtrace

type t = {
  serial : Mutex.t;
  lock : Mutex.t;
  work : Condition.t;
  finished : Condition.t;
  mutable domains : unit Domain.t list;
  mutable generation : int;
  mutable parties : int;
  mutable job : int -> unit;
  mutable pending : int;
  mutable failure : failure option;
  mutable closed : bool;
}

let create () =
  {
    serial = Mutex.create ();
    lock = Mutex.create ();
    work = Condition.create ();
    finished = Condition.create ();
    domains = [];
    generation = 0;
    parties = 1;
    job = ignore;
    pending = 0;
    failure = None;
    closed = false;
  }

let attempt f j =
  match f j with
  | () -> None
  | exception e -> Some (e, Printexc.get_raw_backtrace ())

let rec park t j seen =
  Mutex.lock t.lock;
  while t.generation = seen && not t.closed do
    Condition.wait t.work t.lock
  done;
  let g = t.generation and f = t.job and mine = j < t.parties in
  let closed = t.closed in
  Mutex.unlock t.lock;
  if not closed then begin
    if mine then begin
      let failure = attempt f j in
      Mutex.lock t.lock;
      if Option.is_none t.failure then t.failure <- failure;
      t.pending <- t.pending - 1;
      if t.pending = 0 then Condition.signal t.finished;
      Mutex.unlock t.lock
    end;
    park t j g
  end

let workers t = List.length t.domains

let grow t n =
  let have = workers t in
  if have < n then
    t.domains <-
      t.domains
      @ List.init (n - have) (fun k ->
          let g = t.generation in
          Domain.spawn (fun () -> park t (have + k + 1) g))

let dispatch t d f =
  grow t (d - 1);
  Mutex.lock t.lock;
  t.job <- f;
  t.parties <- d;
  t.pending <- d - 1;
  t.failure <- None;
  t.generation <- t.generation + 1;
  Condition.broadcast t.work;
  Mutex.unlock t.lock;
  let mine = attempt f 0 in
  Mutex.lock t.lock;
  while t.pending > 0 do
    Condition.wait t.finished t.lock
  done;
  let theirs = t.failure in
  t.job <- ignore;
  Mutex.unlock t.lock;
  match (mine, theirs) with
  | Some (e, bt), _ | None, Some (e, bt) -> Printexc.raise_with_backtrace e bt
  | None, None -> ()

let run t d f =
  if d < 1 then invalid_arg "Pool.run: domains must be positive";
  Mutex.protect t.serial (fun () ->
      if t.closed then invalid_arg "Pool.run: pool is shut down";
      if d = 1 then f 0 else dispatch t d f)

let shutdown t =
  Mutex.protect t.serial (fun () ->
      Mutex.lock t.lock;
      t.closed <- true;
      Condition.broadcast t.work;
      Mutex.unlock t.lock;
      List.iter Domain.join t.domains;
      t.domains <- [])

let shared = Atomic.make None

let rec default () =
  match Atomic.get shared with
  | Some t -> t
  | None ->
      ignore (Atomic.compare_and_set shared None (Some (create ())));
      default ()
