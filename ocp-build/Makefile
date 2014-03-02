OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt
OCAMLLEX = ocamllex.opt
OCAMLYACC = ocamlyacc
OCAMLDEP = ocamldep.opt
PP = -pp camlp4o

LIBS = debug lang subcmd min-unix system ocp-build
INCLUDE = $(patsubst %,-I %,$(LIBS))
LINK = $(patsubst %,%.cma,$(LIBS))
NATLINK = $(patsubst %,%.cmxa,$(LIBS))
STUBS = min-unix/minUnix_c.c

all: ocp-build.byte

blob: $(STUBS) */*.mli */*.ml $(*/*.mll:mll=ml) $(*/*.mly:mly=ml)
	$(OCAMLC) -custom $(PP) $(INCLUDE) -I ocp-build min-unix.cma $(STUBS) \
	  `$(OCAMLDEP) $(PP) $(INCLUDE) -I ocp-build -sort $^` -o $@

%.ml: %.mll
	$(OCAMLLEX) $^

%.ml: %.mly
	$(OCAMLYACC) $^

%.cma: %/*.ml %/*.mli
	$(OCAMLC) $(PP) $(INCLUDE) -a `$(OCAMLDEP) $(PP) -sort $^` -o $@

%.cmxa: %/*.ml %/*.mli
	$(OCAMLOPT) $(PP) $(INCLUDE) -a `$(OCAMLDEP) $(PP) -sort $^` -o $@

lang.cma lang.cmxa: lang/ocamllexer.ml
ocp-build.cma ocp-build.cmxa: ocp-build/buildOCPParser.ml ocp-build/metaLexer.ml

ocp-build.byte: $(LINK) ocp-build/*.ml ocp-build/*.mli
	$(OCAMLC) -custom $(PP) $(INCLUDE) unix.cma $(STUBS) $(LINK) buildMain.ml -o $@

ocp-build.native: $(NATLINK) ocp-build/*.ml ocp-build/*.mli
	$(OCAMLOPT) $(PP) $(INCLUDE) unix.cmxa $(STUBS) $(NATLINK) buildMain.ml -o $@

clean:
	rm -f *.a *.cma *.cmxa ocp-build.byte ocp-build.asm */*.cm*
