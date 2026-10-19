(* Structural checks that every game must pass before a solver can trust it. *)

open Cfr

type counts = { histories : int; terminals : int; infosets : int }

module Make (G : Game.S) = struct
  let check () =
    let seen = Hashtbl.create 64 in
    let histories = ref 0 and terminals = ref 0 in
    (* [path] is the list of (infoset, action) pairs taken by each player so
       far. With perfect recall it is the same for every history in an
       infoset. *)
    let rec go h (path0, path1) =
      incr histories;
      match G.node h with
      | Game.Terminal _ -> incr terminals
      | Chance outcomes ->
          let total = List.fold_left (fun acc (p, _) -> acc +. p) 0. outcomes in
          if Float.abs (total -. 1.) > 1e-12 then
            Alcotest.failf "chance probabilities sum to %g" total;
          List.iter
            (fun (p, h') ->
              if p <= 0. then Alcotest.fail "non-positive chance probability";
              go h' (path0, path1))
            outcomes
      | Decision { player; infoset; actions } ->
          let labels = List.map fst actions in
          if List.length (List.sort_uniq compare labels) <> List.length labels
          then Alcotest.failf "duplicate action labels at %s" infoset;
          let own = match player with P0 -> path0 | P1 -> path1 in
          (match Hashtbl.find_opt seen infoset with
          | None -> Hashtbl.add seen infoset (player, labels, own)
          | Some (p, l, o) ->
              if p <> player then
                Alcotest.failf "%s shared by both players" infoset;
              if l <> labels then
                Alcotest.failf "%s has inconsistent actions" infoset;
              if o <> own then Alcotest.failf "%s breaks perfect recall" infoset);
          List.iter
            (fun (a, h') ->
              let step = (infoset, a) :: own in
              go h'
                (match player with P0 -> (step, path1) | P1 -> (path0, step)))
            actions
    in
    go G.root ([], []);
    {
      histories = !histories;
      terminals = !terminals;
      infosets = Hashtbl.length seen;
    }
end
