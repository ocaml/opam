
begin library "extlib"
   sort = true
   files = [
     "extlib/unzip.ml"
     "extlib/uTF8.ml"
     "extlib/uChar.ml"
     "extlib/std.ml"
     "extlib/refList.ml"
     "extlib/pMap.ml"
     "extlib/option.ml"
     "extlib/optParse.ml"
     "extlib/install.ml"
     "extlib/IO.ml"
     "extlib/global.ml"
     "extlib/extString.ml"
     "extlib/extList.ml"
     "extlib/extLib.ml"
     "extlib/extHashtbl.ml"
     "extlib/extArray.ml"
     "extlib/enum.ml"
     "extlib/dynArray.ml"
     "extlib/dllist.ml"
     "extlib/bitSet.ml"
     "extlib/base64.ml"
     ]
end

begin library "re_perl"
   sort = true
   files = [
     "ocaml-re/lib/cset.ml"
     "ocaml-re/lib/automata.ml"
     "ocaml-re/lib/re.ml"
     "ocaml-re/lib/re_perl.ml"
     "ocaml-re/lib/pcre.ml"
   ]
end

begin library "arg"
   files = [
     "ocaml-arg/src/subCommand.ml"
     "ocaml-arg/src/formatExt.ml"
     "ocaml-arg/src/argExt.ml"
   ]
   
end

begin library "cudf"
   sort = true
   files = [
     "cudf/main_cudf_parse_822.ml"
     "cudf/main_cudf_check.ml"
     "cudf/cudf_types_pp.ml"
     "cudf/cudf_types.ml"
     "cudf/cudf_type_parser.mly"
     "cudf/cudf_type_lexer.mll"
     "cudf/cudf_printer.ml"
     "cudf/cudf_parser.ml"
     "cudf/cudf_conf.ml"
     "cudf/cudf_checker.ml"
     "cudf/cudf_c.ml"
     "cudf/cudf_822_parser.mly"
     "cudf/cudf_822_lexer.mll"
     "cudf/cudf.ml"
   ]
   requires = [
     "extlib"
   ]
end

begin library "graph"
  sort = true
  files = [
    "ocamlgraph/lib/bitv.ml"
    "ocamlgraph/lib/unionfind.ml"
    "ocamlgraph/lib/heap.ml"
    pack Graph [
      "ocamlgraph/src/version.ml"
      "ocamlgraph/src/util.ml"
      "ocamlgraph/src/traverse.ml"
      "ocamlgraph/src/topological.ml"
      "ocamlgraph/src/strat.ml"
      "ocamlgraph/src/sig_pack.mli"
      "ocamlgraph/src/sig.mli"
      "ocamlgraph/src/rand.ml"
      "ocamlgraph/src/persistent.ml" (nodeps = [ "Graph" ])
      "ocamlgraph/src/path.ml"
      "ocamlgraph/src/pack.ml"
      "ocamlgraph/src/oper.ml"
      "ocamlgraph/src/minsep.ml"
      "ocamlgraph/src/md.ml"
      "ocamlgraph/src/mcs_m.ml"
      "ocamlgraph/src/kruskal.ml"
      "ocamlgraph/src/imperative.ml"  (nodeps = [ "Graph" ])
      "ocamlgraph/src/graphviz.ml"
      "ocamlgraph/src/gml.mll"
      "ocamlgraph/src/gmap.ml"
      "ocamlgraph/src/flow.ml"
      "ocamlgraph/src/fixpoint.ml"
      "ocamlgraph/src/dot_parser.mly"
      "ocamlgraph/src/dot_lexer.mll"
      "ocamlgraph/src/dot_ast.mli"
      "ocamlgraph/src/dot.ml"
      "ocamlgraph/src/delaunay.ml"
      "ocamlgraph/src/components.ml"
      "ocamlgraph/src/coloring.ml"
      "ocamlgraph/src/cliquetree.ml"
      "ocamlgraph/src/classic.ml"
      "ocamlgraph/src/builder.ml"
      "ocamlgraph/src/blocks.ml"
  ]]
end

begin library "dose"
   sort = true
   pp = "camlp4o Camlp4MacroParser.cmo"
   files = [
     pack Common [
       "dose/common/util.ml"
       "dose/common/url.ml"
       "dose/common/input.ml"
       "dose/common/cudfDiff.ml"
       "dose/common/cudfAdd.ml"
       "dose/common/edosSolver.ml"
(*     "dose/common/minisatSolver.ml" *)
     ]

     pack Debian [
       "dose/deb/format822_parser.mly"
       "dose/deb/format822_lexer.mll"
       "dose/deb/format822.ml"
       "dose/deb/debcudf.ml"
       "dose/deb/packages_parser.mly"
       "dose/deb/packages_lexer.mll"
       "dose/deb/packages.ml"
       "dose/deb/version.ml"
       "dose/deb/sources.ml"
       "dose/deb/release.ml"
       "dose/deb/evolution.ml"
       "dose/deb/edsp.ml"
       "dose/deb/debutil.ml"
       "dose/deb/architecture.ml"
       "dose/deb/apt.ml"
   ]

   pack Algo [
     "dose/algo/strongdeps_int.ml"
     "dose/algo/strongdeps.ml"
     "dose/algo/strongconflicts_int.ml"
     "dose/algo/strongconflicts.ml"
     "dose/algo/statistics.ml"
     "dose/algo/flatten.ml"
     "dose/algo/dominator.ml"
     "dose/algo/dominators.ml"
     "dose/algo/diagnostic_int.ml"
     "dose/algo/diagnostic.ml"
     "dose/algo/depsolver_int.ml"
     "dose/algo/depsolver.ml"
     "dose/algo/defaultgraphs.ml"
   ]]

   requires = [
     "re_perl"
     "extlib"
     "cudf"
     "graph"
     "unix"
   ]

end
