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
	for f in ${MANDIR}/*; do\
	  man2html -r $$f | sed 1,2d > $@/$$(basename $$f .1).html;\
	  echo "  <li><a href=\"$$(basename $$f .1).html\">$$(basename $$f .1)</a></li>" >>$@/index.html;\
	done
	echo '</ul>' >>$@/index.html
	echo '</body></html>' >>$@/index.html

html:
	rm -rf html
	cd .. && $(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) @doc
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
	rm -rf dependencies.dot html/ocamldoc man-html pages/*.html html/index.html
