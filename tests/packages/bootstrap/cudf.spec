@1

package "cudf" {
  version     = "0.6.2"
  description = "http://www.mancoosi.org/reports/tr3.pdf"
  patches = [ "http://www.ocamlpro.com/pub/cudf.tar.bz2"
            ; "local://cudf.install"
            ; "local://cudf.ocp.boot" ]
  make = [ # Sys.command (Printf.sprintf "for i in extlib ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get %s config -I $i | cut -d ' ' -f 2)'\" end' >> cudf.ocp ; done" (match try Some (Unix.getenv "OPAM_ROOT") with Not_found -> None with None -> "" | Some s -> "--root " ^ s)) #
         ; # Sys.command "cat cudf.ocp.boot >> cudf.ocp" #
         ; # let exec s a = Unix.execvp s (Array.append [|s|] a) in exec "ocp-build" [| "-init" ; "-scan" |] # ]
  depends = [ [ ["extlib"] ] ]
}
