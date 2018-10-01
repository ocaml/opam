-include ../Makefile.config

ifeq ($(JBUILDER),)
  JBUILDER_FILE = ../src_ext/jbuilder/_build/install/default/bin/jbuilder$(EXE)
  ifeq ($(shell command -v cygpath 2>/dev/null),)
    JBUILDER := $(JBUILDER_FILE)
  else
    JBUILDER := $(shell echo "$(JBUILDER_FILE)" | cygpath -f - -a)
  endif
else
  JBUILDER_FILE=
endif

ifndef OPAM
  OPAM = $(JBUILDER) exec -- opam
endif

TOPICS  = $(shell $(OPAM) help topics)
TOPICS_ADMIN = cache filter index lint list upgrade

HELPFMT = --help=groff

ifndef OPAM_INSTALLER
  OPAM_INSTALLER = $(JBUILDER) exec -- opam-installer
endif

.PHONY: man html pages
all: man html pages

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
	rm -rf man-html
	mkdir -p $@
	echo '<!DOCTYPE html>' >$@/index.html
	echo '<html><head>' >>$@/index.html
	echo '  <title>Opam man-pages index</title>' >>$@/index.html
	echo '</head><body>' >>$@/index.html
	echo '<h1>Opam $(version) man-pages index</h1>' >>$@/index.html
	echo '<ul>' >>$@/index.html
	for f in man/*; do\
	  man2html -r $$f | sed 1,2d > $@/$$(basename $$f .1).html;\
	  echo "  <li><a href=\"$$(basename $$f .1).html\">$$(basename $$f .1)</a></li>" >>$@/index.html;\
	done
	echo '</ul>' >>$@/index.html
	echo '</body></html>' >>$@/index.html

html:
	rm -rf html
	cd .. && $(JBUILDER) build @doc
	cp -r ../_build/default/_doc/_html html
	sed 's/%{OPAMVERSION}%/'$(version)'/g' index.html > html/index.html
# Not to break older links, add manpages to the `ocamldoc` dir
	mkdir -p html/ocamldoc
	cd html/ocamldoc && for f in ../*/*/index.html; do\
	  ln -sf $$f $$(basename $$(dirname $$f)).html;\
	done

pages/%.html: pages/%.md
	omd $^ -o $@

PAGES=$(wildcard pages/*.md)

pages: $(PAGES:.md=.html)

clean:
	rm -rf dependencies.dot man html/ocamldoc man-html pages/*.html html/index.html
