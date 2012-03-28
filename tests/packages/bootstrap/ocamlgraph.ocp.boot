comp = [ "-g" ]
link = [ "-g" ]

begin library "graph"
  sort = true
  files = [
    "lib/bitv.ml"
    "lib/unionfind.ml"
    "lib/heap.ml"
    pack Graph [
      "src/version.ml"
      "src/util.ml"
      "src/traverse.ml"
      "src/topological.ml"
      "src/strat.ml"
      "src/sig_pack.mli"
      "src/sig.mli"
      "src/rand.ml"
      "src/persistent.ml" (nodeps = [ "Graph" ])
      "src/path.ml"
      "src/pack.ml"
      "src/oper.ml"
      "src/minsep.ml"
      "src/md.ml"
      "src/mcs_m.ml"
      "src/kruskal.ml"
      "src/imperative.ml"  (nodeps = [ "Graph" ])
      "src/graphviz.ml"
      "src/gml.mll"
      "src/gmap.ml"
      "src/flow.ml"
      "src/fixpoint.ml"
      "src/dot_parser.mly"
      "src/dot_lexer.mll"
      "src/dot_ast.mli"
      "src/dot.ml"
      "src/delaunay.ml"
      "src/components.ml"
      "src/coloring.ml"
      "src/cliquetree.ml"
      "src/classic.ml"
      "src/builder.ml"
      "src/blocks.ml"
  ]]
end
