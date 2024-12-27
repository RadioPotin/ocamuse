(lang dune 3.0)

(implicit_transitive_deps false)

(name ocamuse)

(license ISC)

(authors "TODO")

(maintainers "TODO")

(source
 (uri TODO/ocamuse))

(homepage TODO/ocamuse)

(bug_reports TODO/ocamuse)

(documentation TODO/ocamuse)

(generate_opam_files true)

(package
 (name ocamuse)
 (synopsis "A graphical user interface executable")
 (description
  "OCAml MUSE is an interactive GUI tool entirely made in OCaml. Its goal is to ease as much as possible the process of learning music theory on guitar on your own.")
 (tags
  (ocamuse music interactive graphical gui))
 (depends
  (ocaml
   (>= 4.08))
  (lwt
   (>= 5.7.0))
  prelude
  (zed
   (>= 3.2.3))))
