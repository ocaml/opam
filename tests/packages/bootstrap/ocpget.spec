@1

package "ocpget" {
  version     = "max_int"
  description = "https://github.com/OCamlPro/ocp-get"
  urls = [ "git://https://github.com/OCamlPro/ocp-get.git" ]
  patches = [ "install://ocpget.install" ]
  make = [ "make" ]

  depends = "cudf, dose, extlib, ocamlarg, ocamlgraph"
}