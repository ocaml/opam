@1

package "ocpgetboot" {
  version     = "max_int"
  description = "https://github.com/OCamlPro/ocp-get"
  urls = [ "git://https://github.com/OCamlPro/ocp-get.git" ]
  patches = [ "install://ocpgetboot.install"
            ; "ocp://ocpgetboot.ocp.boot" ]
  make = [ "rm ocp-get.ocp"
         ; "for i in extlib ocamlre ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done"
         ; "cat ocpgetboot.ocp.boot >> ocp-get.ocp"
         ; "ocp-build -init -scan" ]
  depends = "extlib, ocamlre"
}