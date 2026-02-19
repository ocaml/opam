.SHELLFLAGS := -eu$(if $(shell (set -o pipefail 2>/dev/null > /dev/null && echo yes)),o pipefail) $(.SHELLFLAGS)

-include ../Makefile.config

ifeq ($(DUNE),)
  DUNE_EXE = ../src_ext/dune-local/_boot/install/default/bin/dune$(EXE)
  ifeq ($(shell command -v cygpath 2>/dev/null),)
    DUNE := $(DUNE_EXE)
  else
    DUNE := $(shell echo "$(DUNE_EXE)" | cygpath -f - -a)
  endif
else
  DUNE_EXE=
endif

DUNE_PROFILE ?= release
DUNE_ARGS ?= 

ifeq ($(DUNE_PROFILE_ARG),release)
  # TODO Replace with --release when we require dune >= 2.5
  DUNE_PROFILE_ARG = --profile=release
else
  DUNE_PROFILE_ARG = --profile=$(DUNE_PROFILE)
endif

.PHONY: man html pages
all: html pages

MANDIR = ../_build/default/doc/man
HTML_MANDIR = man-html

.PHONY: man-markdown
man-markdown: man-html
	rm -rf man-markdown
	mkdir -p $@
	for f in ${HTML_MANDIR}/*.html; do\
	  echo "$$f";\
	  pandoc -t html -w gfm $$f -o $@/$$(basename $$f .html).tmp.md;\
	  echo "---" >>$@/$$(basename $$f .html).tmp2.md;\
	  echo "slug: $$(basename $$f .html).html" >>$@/$$(basename $$f .html).tmp2.md;\
	  echo "title: Manpage $$(basename $$f .html)" >>$@/$$(basename $$f .html).tmp2.md;\
	  echo "description: |" >>$@/$$(basename $$f .html).tmp2.md;\
	  echo "  man page on $$(basename $$f .html)." >>$@/$$(basename $$f .html).tmp2.md;\
	  echo "---" >>$@/$$(basename $$f .html).tmp2.md;\
	  cat $@/$$(basename $$f .html).tmp2.md $@/$$(basename $$f .html).tmp.md >>$@/$$(basename $$f .html).md;\
	  sed -i "s/..\/index.html/index.html/g" $@/$$(basename $$f .html).md;\
	  rm $@/$$(basename $$f .html).tmp.md;\
	  rm $@/$$(basename $$f .html).tmp2.md;\
	done

.PHONY: man-html
man-html:
	rm -rf man-html
	mkdir -p $@
	echo '<!DOCTYPE html>' >$@/index.html
	echo '<html><head>' >>$@/index.html
	echo '  <title>Opam man-pages index</title>' >>$@/index.html
	echo '</head><body>' >>$@/index.html
	echo '<h1>Opam $(version) man-pages index</h1>' >>$@/index.html
	echo '<ul>' >>$@/index.html
	for f in ${MANDIR}/*.1; do\
	  man2html -r $$f | sed 1,2d > $@/$$(basename $$f .1).html;\
	  echo "  <li><a href=\"$$(basename $$f .1).html\">$$(basename $$f .1)</a></li>" >>$@/index.html;\
	done
	echo '</ul>' >>$@/index.html
	echo '</body></html>' >>$@/index.html

html:
	rm -rf html
	cd .. && $(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) @doc
	cp -r ../_build/default/_doc/_html html
	chmod -R +w html
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
	rm -rf dependencies.dot html/ocamldoc man-html man-markdown pages/*.html html/index.html
