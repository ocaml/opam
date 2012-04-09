@1

package "P4" {
  version     = "2"
  description = "Testing constraints"
  patches     = [ "http://www.ocamlpro.com/pub/p4.tar.gz" ]
  depends     = [ [ ["P1";"=";"1"] ]
                ; [ ["P2"] ]
                ; [ ["P3"] ] ]
}
