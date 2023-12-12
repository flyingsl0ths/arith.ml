(lang dune 3.8)

(name arith)

(generate_opam_files true)

(source
 (github flyingsl0ths/arith.ml))

(authors "flyingsl0ths")

(maintainers "flyingsl0ths")

(license GPL-3.0-only)

(package
 (name arith)
 (synopsis "A bc clone")
 (description "A cli calculator")
 (depends ocaml dune (ounit2 :with-test) ppx_deriving)
 )

