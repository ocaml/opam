@1

package "dose" {
  version     = "3bc571a6e029c413004b5f0402366a85b73019a8"
  description = "git://scm.gforge.inria.fr/mancoosi-tools/dose.git"
  urls = [ "http://www.ocamlpro.com/pub/dose.tar.bz2" ]
  patches = [ "install://dose.install"
            ; "ocp://dose.ocp.boot" ]
  make = [ "for i in ocamlre extlib cudf ocamlgraph ocpgetboot ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> dose.ocp ; done"
         ; "cat dose.ocp.boot >> dose.ocp"
         ; "ocp-build -init -scan" ]
  depends = "ocamlre, extlib, cudf, ocamlgraph, ocpgetboot"
}