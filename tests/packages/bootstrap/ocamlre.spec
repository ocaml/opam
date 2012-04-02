@1

package "ocamlre" {
  version     = "191229137d8268e1847e13fbf3bd6522ab062a21"
  description = "https://github.com/avsm/ocaml-re.git"
  sources = [ "http://www.ocamlpro.com/pub/ocaml-re.tar.bz2" ]
  patches = [ "local://ocamlre.install"
            ; "local://ocamlre.ocp.boot" ]
  make = [ "mv -i ocamlre.ocp.boot ocamlre.ocp"
         ; "ocp-build -init -scan" ]
}