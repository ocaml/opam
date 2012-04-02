@1

package "ocpget" {
  version     = "max_int"
  description = "https://github.com/OCamlPro/ocp-get"
  sources = []
  patches = [ "https://github.com/OCamlPro/ocp-get.git"
            ; "local://ocpget.install"
            ; "local://ocpget.ocp.boot" ]
  make = [ "rm depends.ocp ocp-get.ocp lib/bat.ocp"
         ; "for i in cudf dose extlib ocamlarg ocamlgraph ocpgetboot ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done"
         ; "for i in ocpgetboot ; do echo 'begin library \"bat\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done"
         ; "cat ocpget.ocp.boot >> ocp-get.ocp"
         ; "ocp-build -init -scan" ]

  depends = "cudf, dose, extlib, ocamlarg, ocamlgraph, ocpgetboot"
}