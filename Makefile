include Makefile.config

LOCAL_OCPBUILD=./ocp-build/ocp-build
OCPBUILD ?= $(LOCAL_OCPBUILD)
SRC_EXT=src_ext
TARGETS = opam opam-mk-repo

.PHONY: all

all: $(LOCAL_OCPBUILD) META
	$(MAKE) clone
	$(MAKE) compile

scan: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -scan
sanitize: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -sanitize
byte: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -byte
opt: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -asm

$(LOCAL_OCPBUILD): ocp-build/ocp-build.boot ocp-build/win32_c.c
	$(MAKE) -C ocp-build

OCAMLFIND_DIR=$(shell ocamlfind printconf destdir)
prepare: depends.ocp.in
	sed "s|%{lib}%|$(OCAMLFIND_DIR)|g" depends.ocp.in > depends.ocp

compile: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -init -scan -sanitize $(TARGET)

clone:
	$(MAKE) -C $(SRC_EXT)

clean:
	rm -rf _obuild
	rm -f *.annot src/*.annot
	rm -f ocp-build.*
	$(MAKE) -C $(SRC_EXT) clean
	$(MAKE) -C ocp-build clean

distclean: clean
	$(MAKE) -C $(SRC_EXT) distclean
	rm -f META Makefile.config src/core/opamVersion.ml config.log config.status

.PHONY: tests

tests:
	$(MAKE) -C tests all

tests-local:
	$(MAKE) -C tests local

tests-git:
	$(MAKE) -C tests git

%-install:
	@if [ -e _obuild/$*/$*.asm ]; then \
	  echo "install _obuild/$*/$*.asm" && \
	  cp _obuild/$*/$*.asm $(DESTDIR)$(prefix)/bin/$*; \
	else \
	  echo "install _obuild/$*/$*.byte" && \
	  cp _obuild/$*/$*.byte $(DESTDIR)$(prefix)/bin/$*; \
	fi

META: META.in
	sed 's/@VERSION@/$(version)/g' < $< > $@

.PHONY: uninstall install
install:
	mkdir -p $(DESTDIR)$(prefix)/bin
	$(MAKE) $(TARGETS:%=%-install)
	mkdir -p $(DESTDIR)$(mandir)/man1 && cp doc/man/* $(DESTDIR)$(mandir)/man1

uninstall:
	rm -f $(prefix)/bin/opam*
	rm -f $(mandir)/man1/opam*

LIB   = opam-core
NOMLI = opamGlobals.ml
MLI   = $(foreach i, $(shell find src/core -name "*.mli"), $(notdir $i))
_FILES= $(LIB:%=%.a) $(LIB:%=%.cma) $(LIB:%=%.cmxa)\
	$(MLI:%.mli=%.cmi)
FILES = $(_FILES:%=_obuild/opam-core/%) $(NOMLI:%.ml=_obuild/opam-core/%.cmi)

.PHONY: libuninstall libinstall
libinstall: META
	ocamlfind install opam META $(FILES)
libuninstall:
	ocamlfind remove opam

doc: compile
	mkdir -p doc/html/
	ocamldoc \
	  -I _obuild/opam-core -I _obuild/opam-solver \
	  -I _obuild/opam-repositories -I _obuild/opam-client \
	  -I _obuild/opam-lib -I _obuild/cudf -I _obuild/dose \
	  -I _obuild/re -I _obuild/unix -I _obuild/extlib \
	  -I _obuild/arg -I _obuild/graph \
	  src/**/*.mli -html -d doc/html/
	$(MAKE) -C doc

trailing:
	find src -name "*.ml*" -exec \
	  sed -i xxx -e :a -e "/^\n*$$/{$$d;N;ba" -e '}' {} \;
	find src -name "*xxx" -exec rm {} \;

OPAM_FULL  = opam-full-$(version)
OPAM_FILES = $(wildcard src_ext/*.tar.gz)\
	     $(wildcard src_ext/*.tbz)\
	     $(shell git ls-tree --name-only -r HEAD)

archive:
	$(MAKE) -C src_ext distclean
	$(MAKE) clone
	rm -f $(OPAM_FULL) $(OPAM_FULL).tar.gz
	ln -s . $(OPAM_FULL)
	tar cz $(addprefix $(OPAM_FULL)/,$(OPAM_FILES)) > $(OPAM_FULL).tar.gz
	rm -f $(OPAM_FULL)

upload: archive
	read -p "Upload $(OPAM_FULL_TARGZ) [Y/n]?" choice;\
	case x"$$choice" in \
	  x|xy|xY ) scp $(OPAM_FULL).tar.gz webmaster@ocamlpro.com:pub/;;\
	  * ) echo "Cancelled.";;\
	esac

configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf
