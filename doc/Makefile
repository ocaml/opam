OPAM    = ../_obuild/opam/opam.byte
TOPICS  = $(shell $(OPAM) help topics)
HELPFMT = --help=groff

.PHONY: man html dev-manual tutorials
all: man tutorials dev html

man:
	rm -f man/*
	$(OPAM) $(HELPFMT) > man/opam.1
	for i in $(TOPICS); do\
	  $(OPAM) $$i $(HELPFMT) > man/opam-$$i.1;\
	done
	for i in opam-mk-repo opam-check; do\
	  pandoc -s -t man man-ext/$$i.md -o man/$$i.1;\
	done

tutorials:
	$(MAKE) -C tutorials

dev:
	$(MAKE) -C dev-manual

html:
	mkdir -p html/
	ocamldoc \
	  -I ../_obuild/opam-core -I ../_obuild/opam-solver \
	  -I ../_obuild/opam-repositories -I ../_obuild/opam-client \
	  -I ../_obuild/opam-lib -I ../_obuild/cudf -I ../_obuild/dose \
	  -I ../_obuild/re -I ../_obuild/unix -I ../_obuild/extlib \
	  -I ../_obuild/arg -I ../_obuild/graph -I ../_obuild/cmdliner \
	  ../src/**/*.mli -html -d html/
