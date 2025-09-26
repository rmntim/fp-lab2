(lang dune 3.20)

(name labwork2)

(generate_opam_files true)

(source
 (github rmntim/fp-lab2))

(authors "Roman Timoshkin <rmntim@yandex.ru>")

(maintainers "Roman Timoshkin <rmntim@yandex.ru>")

(license MIT)

(package
 (name labwork2)
 (synopsis "Functional Programming Labwork 2")
 (description "Functional Programming Labwork 2")
 (depends
   ocaml
   (zanuda :with-dev-setup)
   (alcotest :with-test)
   (qcheck :with-test)
   (qcheck-alcotest :with-test)))
