ifndef OPAM
  OPAM = ../src/opam
endif
BINDIR = $(dir $(OPAM))
SRCDIR = $(wildcard ../src/*)

TOPICS  = $(shell $(OPAM) help topics)
ifndef OPAM_ADMIN
  OPAM_ADMIN = $(BINDIR)/opam-admin
endif
TOPICS_ADMIN = check make
HELPFMT = --help=groff

SRCEXTDIR = ../src_ext/lib
INCLUDE = $(patsubst %,-I %,$(SRCDIR) $(SRCEXTDIR))

.PHONY: man html dev-manual tutorials
all: man tutorials dev html

man:
	rm -f man/*
	$(OPAM) $(HELPFMT) > man/opam.1
	for i in $(TOPICS); do\
	  $(OPAM) $$i $(HELPFMT) > man/opam-$$i.1;\
	done
	$(OPAM_ADMIN) $(HELPFMT) > man/opam-admin.1
	for i in $(TOPICS_ADMIN); do\
	  $(OPAM_ADMIN) $$i $(HELPFMT) > man/opam-admin-$$i.1;\
	done

tutorials:
	$(MAKE) -C tutorials

dev:
	$(MAKE) -C dev-manual

html:
	mkdir -p html/
	ocamldoc $(INCLUDE) ../src/*/*.mli ../src/*/*.ml -html -d html/
