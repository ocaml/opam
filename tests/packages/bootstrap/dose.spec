@1

package "dose" {
  version     = "3bc571a6e029c413004b5f0402366a85b73019a8"
  description = "git://scm.gforge.inria.fr/mancoosi-tools/dose.git"
  urls = [ "http://www.ocamlpro.com/pub/dose.tar.bz2" ]
  patches = [ "install://dose.install" ]
  make = [ "aclocal -I m4"
         ; "autoconf"
         ; "./configure"
         ; "make" ]
}