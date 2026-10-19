let () =
  Alcotest.run "cfr-solver"
    [
      ("kuhn", Test_kuhn.tests);
      ("regret matching", Test_regret_matching.tests);
      ("strategy", Test_strategy.tests);
    ]
