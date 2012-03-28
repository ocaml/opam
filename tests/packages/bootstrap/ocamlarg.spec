@1

package "ocamlarg" {
  version     = "c1f29093a9f56b79712de58c2c73edb748573f9a"
  description = "https://github.com/samoht/ocaml-arg.git"
  urls = [ "http://www.ocamlpro.com/pub/ocaml-arg.tar.bz2" ]
  patches = [ "install://ocamlarg.install"
            ; "ocp://ocamlarg.ocp.boot" ]
  make = [ "mv -i ocamlarg.ocp.boot ocamlarg.ocp"
         ; "ocp-build -init -scan" ]
}