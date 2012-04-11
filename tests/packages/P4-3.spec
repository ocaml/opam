@1

package "P4" {
  version     = "3"
  description = "Testing transitive closure"
  sources     = [ "http://www.ocamlpro.com/pub/p4.tar.gz" ]
  patches     = [ "http://www.ocamlpro.com/pub/p4.diff"
                ; "file://P4-3_build.sh" ]
  make        = [ # Sys.command "./P4-3_build.sh" # ]
  depends     = [ [ ["P2"] ] ; [ ["P3"] ] ]
}
