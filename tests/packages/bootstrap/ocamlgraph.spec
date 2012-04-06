@1

package "ocamlgraph" {
  version     = "1.8.1"
  description = "http://ocamlgraph.lri.fr/doc"
  patches = [ "http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.1.tar.gz"
            ; "local://ocamlgraph.install"
            ; "local://ocamlgraph.ocp.boot" ]
  make = [ # Unix.rename "ocamlgraph.ocp.boot" "ocamlgraph.ocp" #
         ; # let exec s a = Unix.execvp s (Array.append [|s|] a) in exec "ocp-build" [| "-init" ; "-scan" |] # ]
}