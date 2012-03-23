@1

package "ocpget" {
  version     = "max_int"
  description = "https://github.com/OCamlPro/ocp-get"
  urls = [ "https://github.com/OCamlPro/ocp-get.git" ]

  depends = [ "cudf, dose, extlib, ocamlarg, ocamlgraph" ]
}