@1

package "cudf" {
  version     = "0.6.2"
  description = "http://www.mancoosi.org/reports/tr3.pdf"
  sources = []
  patches = [ "http://www.ocamlpro.com/pub/cudf.tar.bz2"
            ; "local://cudf.install"
            ; "local://cudf.ocp.boot" ]
  make = [ "for i in extlib ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> cudf.ocp ; done"
         ; "cat cudf.ocp.boot >> cudf.ocp"
         ; "ocp-build -init -scan" ]
  depends = "extlib"
}