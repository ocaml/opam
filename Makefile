-include Makefile.config

LOCAL_OCPBUILD=./ocp-build/ocp-build -no-use-ocamlfind
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
	$(OCPBUILD)
byte: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -byte
opt: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -asm

OCAMLBUILD_FLAGS=\
	-Is src/core,src/client,src/repositories,src/solver,src/scripts \
	-use-ocamlfind -pkgs re.glob,re.pcre,re.str,re.perl,ocamlgraph,cmdliner,cudf,dose3 \
	-classic-display
with-ocamlbuild: autogen
	@for i in core repositories solver client; do\
	  echo Compiling opam-$$i;\
	  find src/$$i -type f \( -not -name opamMain.ml \) \
	                       \( -name \*.ml -or -name \*.mly -or -name \*.mll \)\
	    | xargs -n 1 basename\
	    | awk -F. "{ print (toupper(substr(\$$1,0,1)) substr(\$$1,2)) }"\
	    > src/$$i/opam-$$i.mllib &&\
	  ocamlbuild $(OCAMLBUILD_FLAGS) opam-$$i.cma opam-$$i.cmxa;\
	done;\
	ocamlbuild $(OCAMLBUILD_FLAGS) opamMain.native opam_mk_repo.native opam_repo_check.native &&\
	ln -sf _build/src/client/opamMain.native opam &&\
	ln -sf _build/src/scripts/opam_mk_repo.native opam-mk-repo &&\
	ln -sf _build/src/scripts/opam_repo_check.native opam-repo-check

$(LOCAL_OCPBUILD): ocp-build/ocp-build.boot ocp-build/win32_c.c
	$(MAKE) -C ocp-build

OCAMLFIND_DIR=$(shell ocamlfind printconf destdir)
prepare: depends.ocp.in
	sed "s|%{lib}%|$(OCAMLFIND_DIR)|g" depends.ocp.in > depends.ocp

autogen: src/core/opamGitVersion.ml src/core/opamScript.ml src/core/opamVersion.ml

compile: $(LOCAL_OCPBUILD) autogen
	$(OCPBUILD) -init -scan $(TARGET)

clone: src/core/opamVersion.ml
	$(MAKE) -C $(SRC_EXT)

clean:
	rm -rf _obuild
	rm -f *.annot src/*.annot
	rm -f ocp-build.*
	rm -rf _build
	$(MAKE) -C $(SRC_EXT) clean
	$(MAKE) -C ocp-build clean
	rm -f $(OPAM_FULL_TARGZ)

distclean: clean
	$(MAKE) -C $(SRC_EXT) distclean
	rm -f META Makefile.config config.log config.status
	rm -f src/core/opamVersion.ml src/core/opamGitVersion.ml src/core/opamScript.ml
	rm -f doc/man-ext/opam-mk-repo.md doc/man-ext/opam-check.md

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

%-install-with-ocamlbuild:
	@if [ -e $* ]; then\
	  echo "install $*" && cp $* $(DESTDIR)$(prefix)/bin/$*;\
	fi

META: META.in
	sed 's/@VERSION@/$(version)/g' < $< > $@

src/core/opamVersion.ml:
	@echo
	@echo "    ERROR: you need to run ./configure."
	@echo
	@exit 1

.PHONY: src/core/opamGitVersion.ml
src/core/opamGitVersion.ml:
	ocaml shell/get-git-id.ml $@

src/core/opamScript.ml: shell/ src/core/opamVersion.ml
	ocaml shell/crunch.ml "complete"     < shell/opam_completion.sh > $@
	ocaml shell/crunch.ml "complete_zsh" < shell/opam_completion_zsh.sh >> $@
	ocaml shell/crunch.ml "switch_eval"  < shell/opam_switch_eval.sh >> $@

.PHONY: uninstall install install-with-ocamlbuild
install:
	mkdir -p $(DESTDIR)$(prefix)/bin
	$(MAKE) $(TARGETS:%=%-install)
	mkdir -p $(DESTDIR)$(mandir)/man1 && cp doc/man/* $(DESTDIR)$(mandir)/man1
install-with-ocamlbuild:
	mkdir -p $(DESTDIR)$(prefix)/bin
	$(MAKE) $(TARGETS:%=%-install-with-ocamlbuild)
	mkdir -p $(DESTDIR)$(mandir)/man1 && cp doc/man/* $(DESTDIR)$(mandir)/man1
uninstall:
	rm -f $(prefix)/bin/opam*
	rm -f $(mandir)/man1/opam*

CORE_LIB   = opam-core
REPO_LIB   = opam-repositories
SOLVER_LIB = opam-solver
CLIENT_LIB = opam-client

CORE_NOMLI = opamGlobals.ml opamParser.ml opamLexer.ml opamLineLexer.ml
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

OCAMLBUILD_FILES =\
	$(CORE_FILES:%=_build/src/core/%)\
	$(REPO_FILES:%=_build/src/repositories/%)\
	$(SOLVER_FILES:%=_build/src/solver/%)\
	$(CLIENT_FILES:%=_build/src/client/%)

.PHONY: libuninstall libinstall libinstall-with-ocamlbuild
libinstall: META
	ocamlfind install opam META $(FILES)
libinstall-with-ocamlbuild: META
	ocamlfind install opam META $(OCAMLBUILD_FILES)
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

OPAM_FULL       = opam-full-$(version)
OPAM_FULL_TARGZ = $(OPAM_FULL).tar.gz

OPAM_FILES = $(wildcard src_ext/*.tar.gz)\
	     $(wildcard src_ext/*.tbz)\
	     $(shell git ls-tree --name-only -r HEAD)

prepare-archive:
	$(MAKE) -C src_ext distclean
	$(MAKE) clone
	rm -f $(OPAM_FULL) $(OPAM_FULL).tar.gz
	ln -s . $(OPAM_FULL)

# we want OPAM_FILES to be up-to-date here
complete-archive:
	tar cz $(addprefix $(OPAM_FULL)/,$(OPAM_FILES)) > $(OPAM_FULL).tar.gz
	rm -f $(OPAM_FULL)

$(OPAM_FULL_TARGZ):
	$(MAKE) prepare-archive
	$(MAKE) complete-archive

archive: $(OPAM_FULL_TARGZ)
	@
upload: $(OPAM_FULL_TARGZ)
	scp $(OPAM_FULL_TARGZ) webmaster@ocamlpro.com:pub/

configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf

release:
	git tag -d latest || true
	git tag -a latest -m "Latest release"
	git tag -a $(version) -m "Release $(version)"
	$(MAKE) upload
