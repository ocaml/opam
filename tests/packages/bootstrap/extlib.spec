@1

package "extlib" {
  version     = "1.5.2"
  description = "http://ocaml-extlib.googlecode.com/svn/doc/apiref/index.html"
  urls = [ "http://ocaml-extlib.googlecode.com/files/extlib-1.5.2.tar.gz" ]
  patches = [ "install://extlib.install"
            ; "ocp://extlib.ocp.boot" ]
  make = [ "mv -i extlib.ocp.boot extlib.ocp"
         ; "ocp-build -init -scan" ]
}