@1

package "ocpgetboot" {
  version     = "max_int"
  description = "https://github.com/OCamlPro/ocp-get"
  patches = [ "https://github.com/OCamlPro/ocp-get.git"
            ; "local://ocpgetboot.install"
            ; "local://ocpgetboot.ocp.boot" ]
  make = [ # List.iter Unix.unlink [ "depends.ocp" ; "ocp-get.ocp" ] #
         ; # Sys.command (Printf.sprintf "for i in extlib ocamlre ; do echo 'begin library \"'$i'\" dirname = \"'$(ocp-get %s config -I $i | cut -d ' ' -f 2)'\" end' >> ocp-get.ocp ; done" (match try Some (Unix.getenv "OPAM_ROOT") with Not_found -> None with None -> "" | Some s -> "--root " ^ s)) #
         ; # Sys.command "cat ocpgetboot.ocp.boot >> ocp-get.ocp" #
         ; # let exec s a = Unix.execvp s (Array.append [|s|] a) in exec "ocp-build" [| "-init" ; "-scan" |] # ]
  depends = [ [ ["extlib"] ] ; [ ["ocamlre"] ] ]
}
