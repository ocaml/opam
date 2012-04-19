@1

package "P4" {
  version     = "2"
  description = "Testing constraints"
  patches     = [ "http://www.ocamlpro.com/pub/p4.tar.gz"
                ; [ "file://P4-3_build.sh" ; "./P4-2_build.sh" ] ]
  make        = [ # Sys.command "./P4-2_build.sh" # ]
  depends     = [ [ ["P1";"=";"1"] ]
                ; [ ["P2"] ]
                ; [ ["P3"] ] ]
}
