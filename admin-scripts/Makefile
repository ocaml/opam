%: %.ml
	sed 's/^#.*//' $< >$*-tmp.ml
	ocamlfind ocamlc -package opam-lib,opam-lib.repositories -linkpkg ../src/tools/opam_admin_top.ml $*-tmp.ml -o $@
	rm $*-tmp.ml
