-include ../Makefile.config

ifndef OPAM
  OPAM = ../src/opam
endif
BINDIR = $(dir $(OPAM))
SRCDIR = $(wildcard ../src/*)

TOPICS  = $(shell $(OPAM) help topics)

ifndef OPAM_ADMIN
  OPAM_ADMIN = $(BINDIR)/opam-admin
endif
TOPICS_ADMIN = check depexts make rename stats
HELPFMT = --help=groff

ifndef OPAM_INSTALLER
  OPAM_INSTALLER = $(BINDIR)/opam-installer
endif

SRCEXTDIR = ../src_ext/lib

ifneq ($(wildcard $(SRCEXTDIR)),)
  OCAMLDOC = ocamldoc $(patsubst %,-I %,$(SRCDIR) $(SRCEXTDIR))
else
  OCAMLDOC = ocamlfind ocamldoc $(patsubst %,-package %,$(PACKS)) $(patsubst %,-I %,$(SRCDIR))
endif

.PHONY: man html dev-manual pages
all: man dev html pages

man:
	rm -rf man
	mkdir -p man
	$(OPAM) $(HELPFMT) > man/opam.1
	for i in $(TOPICS); do\
	  $(OPAM) $$i $(HELPFMT) > man/opam-$$i.1;\
	done
	$(OPAM_ADMIN) $(HELPFMT) > man/opam-admin.1
	for i in $(TOPICS_ADMIN); do\
	  $(OPAM_ADMIN) $$i $(HELPFMT) > man/opam-admin-$$i.1;\
	done
	$(OPAM_INSTALLER) $(HELPFMT) > man/opam-installer.1

man-html: man
	rm -rf $@
	mkdir -p $@
	for f in $(wildcard man/*); do\
	  man2html -r $$f > man-html/$$(basename $$f .1).html;\
	done

dev:
	$(MAKE) -C dev-manual

html:
	sed 's/%{OPAMVERSION}%/'$(version)'/g' index.html > html/index.html
	rm -rf html/ocamldoc
	mkdir -p html/ocamldoc
	$(OCAMLDOC) ../src/*/*.mli ../src/*/*.ml -html -d html/ocamldoc || true
	$(OCAMLDOC) ../src/*/*.mli ../src/*/*.ml -dot -o /dev/stdout | \
	  tred | \
	  dot -Tsvg > \
	    html/ocamldoc/dependencies.svg \
	|| true

pages/%.html: pages/%.md
	omd $^ -o $@

PAGES=$(wildcard pages/*.md)

pages: $(PAGES:.md=.html)

clean:
	rm -rf man html/ocamldoc man-html pages/*.html
	$(MAKE) -C dev-manual clean
