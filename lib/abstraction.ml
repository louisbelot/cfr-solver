module Make (Full : Game.S) (Abs : Game.S with type history = Full.history) =
struct
  let fail fmt = Printf.ksprintf invalid_arg ("Abstraction: " ^^ fmt)

  let table =
    let seen = Hashtbl.create 1024 and order = ref [] in
    let rec go h =
      match Full.node h with
      | Game.Terminal _ -> ()
      | Chance outcomes -> List.iter (fun (_, h') -> go h') outcomes
      | Decision { player; infoset; actions } ->
          (match Abs.node h with
          | Game.Decision d -> (
              if
                d.player <> player
                || List.map fst d.actions <> List.map fst actions
              then fail "%s and %s differ" infoset d.infoset;
              match Hashtbl.find_opt seen infoset with
              | None ->
                  Hashtbl.add seen infoset d.infoset;
                  order :=
                    (infoset, d.infoset, Array.of_list (List.map fst actions))
                    :: !order
              | Some k when k = d.infoset -> ()
              | Some k -> fail "%s maps to both %s and %s" infoset k d.infoset)
          | _ -> fail "%s is not a decision in %s" infoset Abs.name);
          List.iter (fun (_, h') -> go h') actions
    in
    go Full.root;
    List.rev !order

  let map = List.map (fun (k, a, _) -> (k, a)) table
  let size = List.length (List.sort_uniq compare (List.map snd map))

  let lift s =
    List.fold_left
      (fun acc (k, a, actions) ->
        Strategy.add k ~actions
          ~probs:(Strategy.probs s a ~n:(Array.length actions))
          acc)
      Strategy.empty table
end
