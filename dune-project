(lang dune 3.6)

(name opine)
(version 0.1.3)
(generate_opam_files true)

(source
 (github ArulselvanMadhavan/opine))

(authors "Arulselvan Madhavan")
(maintainers "arulselvan1234@gmail.com")
(license MIT)

(package
 (name opine)
 (synopsis "Python AST unparse implementation in OCaml")
 (description "Opine is an OCaml library that unparses the python AST back to python code.")
 (depends (ocaml (>= 4.14.0)) dune ppx_hash ppx_compare ppx_sexp_conv ppx_make ppx_deriving (pyre-ast (>= 0.1.6)) stdio (base (>= 0.15.0)) uutf (cmdliner (>= 1.1.1)) sexplib ppx_let)
 (tags
  (topics ocaml python ast)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
