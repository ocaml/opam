@1

package "P4" {
  version     = "3"
  description = "Testing transitive closure"
  patches     = [ "http://www.ocamlpro.com/pub/p4.tar.gz"
                ; "http://www.ocamlpro.com/pub/p4.diff" ]
  depends     = "P2, P3"
}
