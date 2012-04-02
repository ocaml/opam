@1

package "ocpgetboot" {
  version     = "max_int"
  description = "https://github.com/OCamlPro/ocp-get"
  patches = [ "https://github.com/OCamlPro/ocp-get.git"
            ; "local://ocpgetboot.install"
            ; "local://ocpgetboot.ocp.boot" ]
  make = [ "rm depends.ocp ocp-get.ocp"
         ; "for i in extlib ocamlre ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done"
         ; "cat ocpgetboot.ocp.boot >> ocp-get.ocp"
         ; "ocp-build -init -scan" ]
  depends = "extlib, ocamlre"
}