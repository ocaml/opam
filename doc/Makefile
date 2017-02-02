-include ../Makefile.config

ifndef OPAM
  OPAM = ../src/opam
endif
BINDIR = $(dir $(OPAM))
SRCDIR = $(sort $(foreach x,$(wildcard ../src/*/*),$(dir $x)))

TOPICS  = $(shell $(OPAM) help topics)
TOPICS_ADMIN = cache filter index lint list upgrade

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
	$(OPAM) admin $(HELPFMT) > man/opam-admin.1
	for i in $(TOPICS_ADMIN); do\
	  $(OPAM) admin $$i $(HELPFMT) > man/opam-admin-$$i.1;\
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
	rm -rf html/ocamldoc
	mkdir -p html/ocamldoc
	sed 's/%{OPAMVERSION}%/'$(version)'/g' index.html > html/index.html
	$(OCAMLDOC) $(wildcard ../src/*/*.mli) $(wildcard ../src/*/*.ml) -html -d html/ocamldoc || true
	$(OCAMLDOC) $(wildcard ../src/*/*.mli) $(wildcard ../src/*/*.ml) -dot -o /dev/stdout | \
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
