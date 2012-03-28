comp = [ "-g" ]
link = [ "-g" ]

begin library "dose"
   sort = true
   pp = "camlp4o Camlp4MacroParser.cmo"
   files = [
     pack Common [
       "common/util.ml"
       "common/url.ml"
       "common/input.ml"
       "common/cudfDiff.ml"
       "common/cudfAdd.ml"
       "common/edosSolver.ml"
(*     "common/minisatSolver.ml" *)
     ]

     pack Debian [
       "deb/format822_parser.mly"
       "deb/format822_lexer.mll"
       "deb/format822.ml"
       "deb/debcudf.ml"
       "deb/packages_parser.mly"
       "deb/packages_lexer.mll"
       "deb/packages.ml"
       "deb/version.ml"
       "deb/sources.ml"
       "deb/release.ml"
       "deb/evolution.ml"
       "deb/edsp.ml"
       "deb/debutil.ml"
       "deb/architecture.ml"
       "deb/apt.ml"
   ]

   pack Algo [
     "algo/strongdeps_int.ml"
     "algo/strongdeps.ml"
     "algo/strongconflicts_int.ml"
     "algo/strongconflicts.ml"
     "algo/statistics.ml"
     "algo/flatten.ml"
     "algo/dominators.ml"
     "algo/diagnostic_int.ml"
     "algo/diagnostic.ml"
     "algo/depsolver_int.ml"
     "algo/depsolver.ml"
     "algo/defaultgraphs.ml"
   ]]

   requires = [
     "ocamlre"
     "extlib"
     "cudf"
     "ocamlgraph"
     "ocpgetboot"
   ]
end
