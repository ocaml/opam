@1

package "ocamlarg" {
  version     = "c1f29093a9f56b79712de58c2c73edb748573f9a"
  description = "https://github.com/samoht/ocaml-arg.git"
  patches = [ "http://www.ocamlpro.com/pub/ocaml-arg.tar.bz2"
            ; "local://ocamlarg.install"
            ; "local://ocamlarg.ocp.boot" ]
  make = [ # Unix.rename "ocamlarg.ocp.boot" "ocamlarg.ocp" #
         ; # let exec s a = Unix.execvp s (Array.append [|s|] a) in exec "ocp-build" [| "-init" ; "-scan" |] # ]
}