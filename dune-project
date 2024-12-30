(lang dune 3.0)

(implicit_transitive_deps false)

(name ocamuse)

(license ISC)

(authors "Dario Pinto <laradiopotin@gmail.com>")

(maintainers "Dario Pinto <laradiopotin@gmail.com>")

(source
 (uri "https://github.com/RadioPotin/ocamuse"))

(homepage "404")

(bug_reports "https://github.com/RadioPotin/ocamuse/issues")

(documentation TODO/ocamuse)

(generate_opam_files true)

(package
 (name ocamuse)
 (synopsis "A graphical user interface executable")
 (description
  "The OCaml Assistant for MUSic Exploration is an interactive GUI tool entirely made in OCaml. Its goal is to ease as much as possible the process of learning music theory on your own. It is based on the lambda-term GUI library - for now only it features a few widgets designed to assist you in learning music theory on a string instruments. E.G.: fretboard display, note patterns and chord diagrams.")
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
