(lang dune 3.16)

(name ocamlfrp)

(generate_opam_files true)

(source
 (github DabrowskiFr/ocamlfrp))

(authors "frederic.dabrowski@univ-orleans.fr")

(maintainers "frederic.dabrowski@univ-orleans.fr")

(license AGPL-3.0-only)

(documentation https://dabrowskifr.github.io/ocamlfrp/)

(package
 (name ocamlfrp)
 (synopsis "An Ocaml Arrowized FRP library")
 (description "An Ocaml Arrowized FRP library based on coiterators")
 (depends ocaml)
 (tags
  ("functional reactive programming" "arrows" "coiterators" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
