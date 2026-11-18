open Multi_tree

type history = {
  node : node;
  rho : int array;
  seen : int list array;
  off : bool array;
  pending : int option;
}

let key tree k h p =
  Printf.sprintf "%s#%s%s" tree.keys.(k)
    (String.concat "." (List.rev_map string_of_int h.seen.(p)))
    (if h.off.(p) then "#off" else "")

let game (tree : Multi_tree.t) mass =
  if tree.players <> 2 then invalid_arg "Mediator.game: two players only";
  if Array.exists (fun k -> String.contains k '#') tree.keys then
    invalid_arg "Mediator.game: '#' in an infoset key";
  let joint p own other = if p = 0 then mass own other else mass other own in
  let node h =
    match (h.node, h.pending) with
    | Leaf u, _ -> Multi.Terminal u
    | Chance _, _ -> invalid_arg "Mediator.game: chance nodes"
    | Decision (k, children), _ when h.off.(tree.owner.(k)) ->
        let p = tree.owner.(k) in
        Decision
          {
            player = p;
            infoset = key tree k h p;
            actions =
              List.mapi
                (fun a c -> (tree.labels.(k).(a), { h with node = c }))
                (Array.to_list children);
          }
    | Decision (k, children), None ->
        let p = tree.owner.(k) in
        let n = Array.length children in
        let w =
          Array.init n (fun a ->
              Float.max 0. (joint p (tree.offset.(k) + a) h.rho.(1 - p)))
        in
        let total = Array.fold_left ( +. ) 0. w in
        Chance
          (List.init n (fun a ->
               let q =
                 if total > 1e-12 then w.(a) /. total else 1. /. float_of_int n
               in
               let seen = Array.copy h.seen in
               seen.(p) <- a :: seen.(p);
               (q, { h with seen; pending = Some a })))
    | Decision (k, children), Some r ->
        let p = tree.owner.(k) in
        Decision
          {
            player = p;
            infoset = key tree k h p;
            actions =
              List.mapi
                (fun a c ->
                  let rho = Array.copy h.rho and off = Array.copy h.off in
                  rho.(p) <- tree.offset.(k) + r;
                  off.(p) <- a <> r;
                  ( tree.labels.(k).(a),
                    { h with node = c; rho; off; pending = None } ))
                (Array.to_list children);
          }
  in
  (module struct
    type nonrec history = history

    let name = "mediated"
    let players = 2

    let root =
      {
        node = tree.root;
        rho = [| -1; -1 |];
        seen = [| []; [] |];
        off = [| false; false |];
        pending = None;
      }

    let node = node
  end : Multi.S)

let recommended key =
  match String.split_on_char '#' key with
  | [ _; recs ] when recs <> "" ->
      Some (int_of_string (List.hd (List.rev (String.split_on_char '.' recs))))
  | _ -> None

let direct (tree : Multi_tree.t) =
  let f = Array.make (slots tree) 0. in
  Array.iteri
    (fun k key ->
      let lo = tree.offset.(k) and hi = tree.offset.(k + 1) in
      match recommended key with
      | Some r -> f.(lo + r) <- 1.
      | None ->
          for s = lo to hi - 1 do
            f.(s) <- 1. /. float_of_int (hi - lo)
          done)
    tree.keys;
  f
