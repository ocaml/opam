@1

package "dose" {
  version     = "3bc571a6e029c413004b5f0402366a85b73019a8"
  description = "git://scm.gforge.inria.fr/mancoosi-tools/dose.git"
  patches = [ "http://www.ocamlpro.com/pub/dose.tar.bz2"
            ; "local://dose.install"
            ; "local://dose.ocp.boot" ]
  make = [ # Sys.command "for i in ocamlre extlib cudf ocamlgraph ocpgetboot ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> dose.ocp ; done" #
         ; # Sys.command "cat dose.ocp.boot >> dose.ocp" #
         ; # let exec s a = Unix.execvp s (Array.append [|s|] a) in exec "ocp-build" [| "-init" ; "-scan" |] # ]
  depends = "ocamlre, extlib, cudf, ocamlgraph, ocpgetboot"
}