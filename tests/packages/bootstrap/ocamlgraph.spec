@1

package "ocamlgraph" {
  version     = "1.8.1"
  description = "http://ocamlgraph.lri.fr/doc"
  urls = [ "http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.1.tar.gz" ]
  patches = [ "local://ocamlgraph.install"
            ; "local://ocamlgraph.ocp.boot" ]
  make = [ "mv -i ocamlgraph.ocp.boot ocamlgraph.ocp"
         ; "ocp-build -init -scan" ]
}