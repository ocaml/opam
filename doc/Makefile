ifndef OPAM
  OPAM = ../src/opam
endif
BINDIR = $(dir $(OPAM))
SRCDIR = $(wildcard ../src/*)

TOPICS  = $(shell $(OPAM) help topics)

ifndef OPAM_ADMIN
  OPAM_ADMIN = $(BINDIR)/opam-admin
endif
TOPICS_ADMIN = check make depexts libs stats
HELPFMT = --help=groff

ifndef OPAM_INSTALLER
  OPAM_INSTALLER = $(BINDIR)/opam-installer
endif

ifndef OPAM_PUBLISH
  OPAM_PUBLISH = $(BINDIR)/opam-publish
endif
TOPICS_PUBLISH = prepare submit

SRCEXTDIR = ../src_ext/lib
INCLUDE = $(patsubst %,-I %,$(SRCDIR) $(SRCEXTDIR))

.PHONY: man html dev-manual tutorials pages
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
	$(OPAM_INSTALLER) $(HELPFMT) > man/opam-installer.1
	for i in $(TOPICS_PUBLISH); do\
	  $(OPAM_PUBLISH) $$i $(HELPFMT) > man/opam-publish-$$i.1;\
	done

man-html: man
	rm -rf $@
	mkdir -p $@
	for f in $(wildcard man/*); do\
	  man2html -r $$f > man-html/$$(basename $$f .1).html;\
	done

tutorials:
	$(MAKE) -C tutorials

dev:
	$(MAKE) -C dev-manual

html:
	rm -rf html
	mkdir -p html/
	ocamldoc $(INCLUDE) ../src/*/*.mli ../src/*/*.ml -html -d html/ || true

pages/%.html: pages/%.md
	omd $^ -o $@

PAGES=$(wildcard pages/*.md)

pages: $(PAGES:.md=.html)

clean:
	rm -rf man html man-html pages/*.html
	$(MAKE) -C dev-manual clean
	$(MAKE) -C tutorials clean
