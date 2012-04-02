@1

package "extlib" {
  version     = "1.5.2"
  description = "http://ocaml-extlib.googlecode.com/svn/doc/apiref/index.html"
  patches = [ "http://ocaml-extlib.googlecode.com/files/extlib-1.5.2.tar.gz"
            ; "local://extlib.install"
            ; "local://extlib.ocp.boot" ]
  make = [ "mv -i extlib.ocp.boot extlib.ocp"
         ; "ocp-build -init -scan" ]
}