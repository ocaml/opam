@1

package "ocpget" {
  version     = "max_int"
  description = "https://github.com/OCamlPro/ocp-get"
  patches = [ "https://github.com/OCamlPro/ocp-get.git"
            ; "local://ocpget.install"
            ; "local://ocpget.ocp.boot" ]
  make = [ # List.iter Unix.unlink [ "depends.ocp" ; "ocp-get.ocp" ; "lib/bat.ocp" ] #
         ; # Sys.command "for i in cudf dose extlib ocamlarg ocamlgraph ocpgetboot ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done" #
         ; # Sys.command "for i in ocpgetboot ; do echo 'begin library \"bat\" dirname = \"'$(ocp-get --root /tmp/OPAM.TEST config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done" #
         ; # Sys.command "cat ocpget.ocp.boot >> ocp-get.ocp" #
         ; # let exec s a = Unix.execvp s (Array.append [|s|] a) in exec "ocp-build" [| "-init" ; "-scan" |] # ]
  depends = [ [ ["cudf"] ] ; [ ["dose"] ] ; [ ["extlib"] ] ; [ ["ocamlarg"] ] ; [ ["ocamlgraph"] ] ; [ ["ocpgetboot"] ] ]
}
