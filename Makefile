-include Makefile.config

LOCAL_OCPBUILD=./ocp-build/ocp-build
OCPBUILD ?= $(LOCAL_OCPBUILD)
SRC_EXT=src_ext
TARGETS = opam opam-mk-repo

.PHONY: all

all: $(LOCAL_OCPBUILD) META src/core/opamGitVersion.ml
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

ISGIT = $(shell git status > /dev/null && if [ $$? -eq 0 ]; then echo "1"; else echo "0"; fi)

ifeq ($(ISGIT), 1)
src/core/opamGitVersion.ml: .git/logs/HEAD
	@echo 'let version = Some "$(shell git describe)"' > $@
else
src/core/opamGitVersion.ml:
	@echo "let version = None" > $@
endif

.PHONY: uninstall install
install:
	mkdir -p $(DESTDIR)$(prefix)/bin
	$(MAKE) $(TARGETS:%=%-install)
	mkdir -p $(DESTDIR)$(mandir)/man1 && cp doc/man/* $(DESTDIR)$(mandir)/man1

uninstall:
	rm -f $(prefix)/bin/opam*
	rm -f $(mandir)/man1/opam*

CORE_LIB   = opam-core
REPO_LIB   = opam-repositories
SOLVER_LIB = opam-solver
CLIENT_LIB = opam-client

CORE_NOMLI = opamGlobals.ml
CORE_MLI   = $(foreach i, $(shell find src/core -name "*.mli"), $(notdir $i))
REPO_MLI   = $(foreach i, $(shell find src/repositories -name "*.mli"), $(notdir $i))
SOLVER_MLI = $(foreach i, $(shell find src/solver -name "*.mli"), $(notdir $i))
CLIENT_MLI = $(foreach i, $(shell find src/client -name "*.mli"), $(notdir $i))

CORE_FILES   = $(CORE_LIB:%=%.a) $(CORE_LIB:%=%.cma) $(CORE_LIB:%=%.cmxa)\
	       $(CORE_MLI:%.mli=%.cmi) $(CORE_NOMLI:%.ml=%.cmi)
REPO_FILES   = $(REPO_LIB:%=%.a) $(REPO_LIB:%=%.cma) $(REPO_LIB:%=%.cmxa)\
	       $(REPO_MLI:%.mli=%.cmi)
SOLVER_FILES = $(SOLVER_LIB:%=%.a) $(SOLVER_LIB:%=%.cma) $(SOLVER_LIB:%=%.cmxa)\
	       $(SOLVER_MLI:%.mli=%.cmi)
CLIENT_FILES = $(CLIENT_LIB:%=%.a) $(CLIENT_LIB:%=%.cma) $(CLIENT_LIB:%=%.cmxa)\
	       $(CLIENT_MLI:%.mli=%.cmi)

FILES = $(CORE_FILES:%=_obuild/opam-core/%)\
	$(REPO_FILES:%=_obuild/opam-repositories/%)\
	$(SOLVER_FILES:%=_obuild/opam-solver/%)\
	$(CLIENT_FILES:%=_obuild/opam-client/%)

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
