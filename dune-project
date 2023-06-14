(lang dune 3.7)

(name vase)

(generate_opam_files true)

(source
 (github jbachurski/vase))

(authors "Jakub Bachurski")

(maintainers "Jakub Bachurski")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name vase)
 (synopsis "Experimenting with a compiler")
 (depends ocaml dune alcotest))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
