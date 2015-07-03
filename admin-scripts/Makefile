%: %.ml
	sed 's/^#.*//' $< >$*-tmp.ml
	ocamlfind ocamlc -package opam-lib,opam-lib.repository -linkpkg ../src/tools/opam_admin_top.ml $*-tmp.ml -o $@
	rm $*-tmp.ml

couverture: couverture.ml
	sed 's/^#.*//' $< >couverture-tmp.ml
	ocamlfind ocamlopt -package re.glob,opam-lib.state -linkpkg ../src/tools/opam_admin_top.ml couverture-tmp.ml -o $@
	rm couverture-tmp.ml
