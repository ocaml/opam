@1

package "P2" {
  version     = "1"
  description =
    "An other very useful package.\

     The description can go on multiple lines but they\
     need to be escaped correclty (see the '\\' character\
     at the end of the lines."

  depends = [ [ ["P1"] ] ]
}
