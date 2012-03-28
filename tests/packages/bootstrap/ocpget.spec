@1

package "ocpget" {
  version     = "max_int"
  description = "https://github.com/OCamlPro/ocp-get"
  urls = [ "git://https://github.com/OCamlPro/ocp-get.git" ]
  patches = [ "install://ocpget.install"
            ; "ocp://ocpget.ocp.boot" ]
  make = [ "rm ocp-get.ocp lib/bat.ocp"
         ; "for i in cudf dose extlib ocamlarg ocamlgraph ocpgetboot ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done"
         ; "for i in ocpgetboot ; do echo 'begin library \"bat\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done"
         ; "cat ocpget.ocp.boot >> ocp-get.ocp"
         ; "ocp-build -init -scan" ]

  depends = "cudf, dose, extlib, ocamlarg, ocamlgraph, ocpgetboot"
}