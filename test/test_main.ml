let () =
  Alcotest.run "cfr-solver"
    [
      ("kuhn", Test_kuhn.tests);
      ("regret matching", Test_regret_matching.tests);
      ("strategy", Test_strategy.tests);
      ("eval", Test_eval.tests);
      ("cfr", Test_cfr.tests);
      ("best response", Test_best_response.tests);
    ]
