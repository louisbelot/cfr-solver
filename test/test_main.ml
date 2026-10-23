let () =
  Alcotest.run "cfr-solver"
    [
      ("kuhn", Test_kuhn.tests);
      ("leduc", Test_leduc.tests);
      ("abstraction", Test_abstraction.tests);
      ("strength", Test_strength.tests);
      ("cluster", Test_cluster.tests);
      ("hand", Test_hand.tests);
      ("regret matching", Test_regret_matching.tests);
      ("strategy", Test_strategy.tests);
      ("eval", Test_eval.tests);
      ("tree", Test_tree.tests);
      ("cfr", Test_cfr.tests);
      ("mccfr", Test_mccfr.tests);
      ("weighting", Test_weighting.tests);
      ("rng", Test_rng.tests);
      ("barrier", Test_barrier.tests);
      ("pool", Test_pool.tests);
      ("parallel", Test_parallel.tests);
      ("best response", Test_best_response.tests);
      ("json", Test_json.tests);
    ]
