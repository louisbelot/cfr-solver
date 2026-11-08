let () =
  Alcotest.run "cfr-solver"
    [
      ("kuhn", Test_kuhn.tests);
      ("figgie", Test_figgie.tests);
      ("leduc", Test_leduc.tests);
      ("liars dice", Test_liars_dice.tests);
      ("multi", Test_multi.tests);
      ("multi br", Test_multi_br.tests);
      ("multi mccfr", Test_multi_mccfr.tests);
      ("multi profile", Test_multi_profile.tests);
      ("leduc n", Test_leduc_n.tests);
      ("play", Test_play.tests);
      ("abstraction", Test_abstraction.tests);
      ("strength", Test_strength.tests);
      ("cluster", Test_cluster.tests);
      ("hand", Test_hand.tests);
      ("range", Test_range.tests);
      ("suit", Test_suit.tests);
      ("equity", Test_equity.tests);
      ("holdem buckets", Test_holdem_buckets.tests);
      ("bet tree", Test_bet_tree.tests);
      ("subgame", Test_subgame.tests);
      ("resolve", Test_resolve.tests);
      ("translation", Test_translation.tests);
      ("regret matching", Test_regret_matching.tests);
      ("strategy", Test_strategy.tests);
      ("eval", Test_eval.tests);
      ("tree", Test_tree.tests);
      ("cfr", Test_cfr.tests);
      ("dilated", Test_dilated.tests);
      ("mccfr", Test_mccfr.tests);
      ("weighting", Test_weighting.tests);
      ("rng", Test_rng.tests);
      ("barrier", Test_barrier.tests);
      ("pool", Test_pool.tests);
      ("parallel", Test_parallel.tests);
      ("best response", Test_best_response.tests);
      ("json", Test_json.tests);
      ("bigint", Test_bigint.tests);
      ("rat", Test_rat.tests);
      ("lp", Test_lp.tests);
      ("sequence form", Test_sequence_form.tests);
      ("auction", Test_auction.tests);
      ("market", Test_market.tests);
    ]
