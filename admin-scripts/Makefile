DEPS = core format repository

INCLUDE = $(patsubst %,-I ../src/%,$(DEPS)) -I ../src/tools

LIBS = $(patsubst %,../src/opam-%.cma,$(DEPS))

%: %.ml
	sed 's/^#.*//' $< >$*-tmp.ml
	ocamlfind ocamlc -package unix,re.glob,ocamlgraph -linkpkg $(INCLUDE) $(LIBS) ../src/tools/opam_admin_top.ml $*-tmp.ml -o $@
	rm $*-tmp.ml

1_2_to_2_0: compilers-to-packages
	cp $< $@

couverture: couverture.ml
	sed 's/^#.*//' $< >couverture-tmp.ml
	ocamlfind ocamlopt -package re.glob,opam-lib.state -linkpkg ../src/tools/opam_admin_top.ml couverture-tmp.ml -o $@
	rm couverture-tmp.ml
