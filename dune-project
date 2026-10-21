(lang dune 3.0)

(name cfr-solver)

(generate_opam_files true)

(source
 (github louisbelot/cfr-solver))

(authors "Louis Belot")

(maintainers "Louis Belot")

(license MIT)

(package
 (name cfr-solver)
 (synopsis "Counterfactual regret minimization for imperfect-information games")
 (description
  "Nash-equilibrium strategies for Kuhn poker and other imperfect-information games, computed with counterfactual regret minimization, with exact best responses and exploitability.")
 (depends
  (ocaml
   (>= 5.1))
  (dune
   (>= 3.0))
  (cmdliner
   (>= 1.3.0))
  (alcotest :with-test)
  (qcheck-core :with-test)))
