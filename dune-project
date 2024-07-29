(lang dune 2.0)
(name opam)

(implicit_transitive_deps true)

(package (name opam-core))
(package (name opam-format))
(package (name opam-repository))
(package (name opam-state))
(package (name opam-solver))
(package (name opam-client))
(package (name opam-devel))
(package (name opam-installer))

; Meta packages
(package (name opam))
(package (name opam-admin))
