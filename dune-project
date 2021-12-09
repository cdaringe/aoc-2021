(lang dune 2.9)

(name aoc)
; version field is optional
(version 1.0.0)

(generate_opam_files true)

(source (github cdaringe/aoc_2021))
(license ISC)
(authors "cdaringe")
(maintainers "cdaringe")

(package
 (name aoc)
 (synopsis "aoc")
 (description "aoc")
 (depends
  (alcotest :with-test)
  (ppx_string :with-test)
  (dune (> 2))
  (containers (and :with-test (>= 3.6.1)))
  ))
