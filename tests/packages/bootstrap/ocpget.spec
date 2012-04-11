@1

package "ocpget" {
  version     = "max_int"
  description = "https://github.com/OCamlPro/ocp-get"
  patches = [ "https://github.com/OCamlPro/ocp-get.git"
            ; "local://ocpget.install"
            ; "local://ocpget.ocp.boot" ]
  make = [ # List.iter Unix.unlink [ "depends.ocp" ; "ocp-get.ocp" ; "lib/bat.ocp" ] #
         ; # let ocpget_root = match try Some (Unix.getenv "OPAM_ROOT") with Not_found -> None with None -> "" | Some s -> "--root " ^ s in let err = Sys.command (Printf.sprintf "for i in cudf dose extlib ocamlarg ocamlgraph ocpgetboot ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get %s config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done" ocpget_root) in if err = 0 then Sys.command (Printf.sprintf "for i in ocpgetboot ; do echo 'begin library \"bat\" dirname = \"'$(ocp-get %s config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done" ocpget_root) else exit 1 #
         ; # Sys.command "cat ocpget.ocp.boot >> ocp-get.ocp" #
         ; # let exec s a = Unix.execvp s (Array.append [|s|] a) in exec "ocp-build" [| "-init" ; "-scan" |] # ]
  depends = [ [ ["cudf"] ] ; [ ["dose"] ] ; [ ["extlib"] ] ; [ ["ocamlarg"] ] ; [ ["ocamlgraph"] ] ; [ ["ocpgetboot"] ] ]
}
