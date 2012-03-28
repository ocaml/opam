comp = [ "-g" ]
link = [ "-g" ]

begin library "ocp-get-lib"
(*
  ocamlc   = [ "ocp-ocamlc.opt" ]
  ocamlopt = [ "ocp-ocamlopt.opt" ]
*)
  dirname = [ "src" ]
  comp   += [ "-annot" "-warn-error" "A" ]
  files   = [
    "globals.ml"
    "uri.ml"
    "namespace.ml"
    "run.ml"
    "file_format.ml"
    "lexer.mll"
    "parser.mly"
    "path.ml"
    "file.ml"
    "protocol.ml"
    "server.ml" 
  ]

  requires = [
    "cudf"
    "dose"
    "bat"
    "unix"
    "extlib"
    "ocamlarg"
    "ocamlgraph"
  ]
end

begin library "ocp-get"
(*
  ocamlc   = [ "ocp-ocamlc.opt" ]
  ocamlopt = [ "ocp-ocamlopt.opt" ]
*)
  dirname  = [ "src" ]
  comp    += [ "-annot" "-warn-error" "A" ]
  files    = [
    "solver.ml"
    "client.ml"
    "ocp_get.ml"
  ]
  requires = [ "ocp-get-lib" ]
end

begin library "ocp-get-server"
(*
  ocamlc   = [ "ocp-ocamlc.opt" ]
  ocamlopt = [ "ocp-ocamlopt.opt" ]
*)
  dirname  = [ "src" ]
  comp    += [ "-annot" "-warn-error" "A" ]
  files    = [ "ocp_get_server.ml" ]
  requires = [ "ocp-get-lib" ]
end
