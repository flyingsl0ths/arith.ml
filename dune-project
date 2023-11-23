(lang dune 3.8)

(name arith)

(generate_opam_files true)

(source
 (github flyingsl0ths/arith.ml))

(authors "flyingsl0ths")

(maintainers "flyingsl0ths")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name arith)
 (synopsis "A bc clone")
 (description "A cli calculator")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
